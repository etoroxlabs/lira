-- MIT License
-- 
-- Copyright (c) 2019 eToroX Labs
-- 
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
-- 
-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.
-- 
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

module Lira.Backends.Evm.EvmCompiler
  ( -- External interface
    assemble
  , ppEvm
  , runCompiler
  , runExprCompiler

    -- Internals exposed for testing
  , initialEnv
  , push
  , linker
  , transformPseudoInstructions
  ) where

import           Lira.Contract hiding ( Transfer )
import           Lira.Contract.Intermediate
import           Lira.Backends.IntermediateCompiler ( emptyContract )
import           Lira.Backends.Evm.EvmLanguageDefinition

import           Control.Monad.State

import           Data.List                      ( genericLength )
import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as Text
import           Data.Text                      ( Text )
import           Data.Word                      ( Word8, Word32 )
import           Crypto.Hash (Keccak_256, Digest, hash)
import           Data.ByteString                ( ByteString )
import           Data.ByteString.Char8          ( pack )
import           Data.Char                      ( ord )
import           Numeric                        ( showHex )
import           Text.Printf                    ( printf )

-- State monad definitions
data CompileEnv = CompileEnv
  { labelCount        :: Integer
  , transferCallCount :: Integer
  , memOffset         :: Integer
  , labelString       :: String
  , ceContract        :: IntermediateContract
  } deriving Show

type Compiler a = State CompileEnv a

initialEnv :: CompileEnv
initialEnv = CompileEnv
  { labelCount        = 0
  , transferCallCount = 0
  , memOffset         = 0
  , labelString       = "mem_exp"
  , ceContract        = emptyContract
  }

-- ATM, "Executed" does not have an integer. If it should be able to handle more
-- than 256 tcalls, it must take an integer also.
data StorageType = CreationTimestamp
                 | Executed
                 | Activated
                 | MemoryExpressionRefs
                 | PartyMap
                 | PartyFreeMap

-- For each storage index we pay 20000 GAS. Reusing one is only 5000 GAS.
-- It would therefore make sense to pack as much as possible into the same index.
-- Storage is word addressed, not byte addressed
storageAddress :: Integral i => StorageType -> i
storageAddress CreationTimestamp    = 0x0
storageAddress Activated            = 0x1
storageAddress Executed             = 0x2
storageAddress MemoryExpressionRefs = 0x3
storageAddress PartyMap             = 0x4
storageAddress PartyFreeMap         = 0x5

sizeOfOpcodes :: [EvmOpcode] -> Integer
sizeOfOpcodes = sum . map sizeOfOpcode
-- This function is called before the linker and before the
-- elimination of pseudo instructions, so it must be able to
-- also handle the pseudo instructions before and after linking
sizeOfOpcode :: EvmOpcode -> Integer
sizeOfOpcode (PUSH1    _   ) = 2
sizeOfOpcode (PUSH4    _   ) = 5
sizeOfOpcode (PUSH32   _   ) = 33
sizeOfOpcode (PUSHN    ws  ) = genericLength ws + 1
sizeOfOpcode (JUMPITO  _   ) = 1 + 5 -- PUSH4 addr.; JUMPI
sizeOfOpcode (JUMPTO   _   ) = 1 + 5 -- PUSH4 addr.; JUMP
sizeOfOpcode (JUMPITOA _   ) = 1 + 5 -- PUSH4 addr.; JUMP
sizeOfOpcode (JUMPTOA  _   ) = 1 + 5 -- PUSH4 addr.; JUMP
sizeOfOpcode (FUNSTART _ _n) = 1 + 1 -- JUMPDEST; SWAPn
-- PC stores in µ[0] PC before PC opcode, we want to store the address
-- pointing to the OPCODE after the JUMP opcode. Therefore, we add 10 to byte code address
sizeOfOpcode (FUNCALL _    ) = 4 + 7 -- PC; PUSH1 10, ADD, JUMPTO label; JUMPDEST = PC; PUSH1, ADD, PUSH4 addr; JUMP; JUMPDEST; OPCODE -- addr(OPCODE)=µ[0]
sizeOfOpcode FUNRETURN       = 2 -- SWAP1; JUMP;
sizeOfOpcode _               = 1

----------------------------------------------------------------------------
-- Main method for this module. Returns binary.
-- Check that there are not more than 2^8 transfercalls
assemble :: IntermediateContract -> Text
assemble
  = Text.pack
  . concatMap ppEvm
  . transformPseudoInstructions
  . compile 
  . check

check :: IntermediateContract -> IntermediateContract
check contract
  | length (transferCalls contract) > 256 = error "Too many Transfer Calls"
  | length (memExps contract) > 128 = error "Too many Memory Expressions"
  | otherwise = contract

-- Given an IntermediateContract, returns the EvmOpcodes representing the contract
compile :: IntermediateContract -> [EvmOpcode]
compile intermediateContract = linker (constructor' ++ codecopy') ++ linker body
 where
  constructor' = constructor intermediateContract
  codecopy'    = codecopy constructor' body
  body         = jumpTable ++ subroutines ++ activateCheck ++ execute' ++ activate' ++ take'
  execute'     = runCompiler executeBody -- also contains selfdestruct when contract is fully executed
  activate'    = runCompiler activateBody
  take'        = runCompiler takeBody

    -- The addresses of the constructor run are different from runs when DC is on BC

runCompiler :: Compiler [EvmOpcode] -> [EvmOpcode]
runCompiler = (`evalState` initialEnv)

runExprCompiler :: CompileEnv -> Expr -> [EvmOpcode]
runExprCompiler env = (`evalState` env) . compileExp

linker :: [EvmOpcode] -> [EvmOpcode]
linker opcodes' = linkerH 0 opcodes' opcodes'
 where
  linkerH :: Integer -> [EvmOpcode] -> [EvmOpcode] -> [EvmOpcode]
  linkerH _ relabeledOpcodes [] = relabeledOpcodes
  linkerH instructionCount accOpcodes (opcode : opcodes) = case opcode of
    JUMPDESTFROM label -> linkerH
      (instructionCount + 1)
      (replaceLabel label instructionCount accOpcodes)
      opcodes
    FUNSTART label _ -> linkerH
      (instructionCount + 2)
      (replaceLabel label instructionCount accOpcodes)
      opcodes
    _ -> linkerH (instructionCount + n) accOpcodes opcodes
      where n = sizeOfOpcode opcode

replaceLabel :: Label -> Integer -> [EvmOpcode] -> [EvmOpcode]
replaceLabel label int = map replaceH
 where
  replaceH :: EvmOpcode -> EvmOpcode
  replaceH opcode = case opcode of
    JUMPTO       label' -> if label == label' then JUMPTOA int else opcode
    JUMPITO      label' -> if label == label' then JUMPITOA int else opcode
    JUMPDESTFROM label' -> if label == label' then JUMPDEST else opcode
    FUNSTART label' n   -> if label == label' then FUNSTARTA n else opcode
    FUNCALL label'      -> if label == label' then FUNCALLA int else opcode
    _                   -> opcode

transformPseudoInstructions :: [EvmOpcode] -> [EvmOpcode]
transformPseudoInstructions = concatMap transformH
 where
  transformH :: EvmOpcode -> [EvmOpcode]
  transformH opcode = case opcode of
    JUMPTOA   i -> [PUSH4 (fromInteger i), JUMP]
    JUMPITOA  i -> [PUSH4 (fromInteger i), JUMPI]
    FUNCALLA  i -> [PC, PUSH1 10, ADD, PUSH4 (fromInteger i), JUMP, JUMPDEST]
    FUNSTARTA n -> [JUMPDEST, swap n]
    FUNRETURN   -> [SWAP1, JUMP]
    _           -> [opcode]

  swap :: Integer -> EvmOpcode
  swap 2 = SWAP2
  swap 3 = SWAP3
  swap _ = undefined -- Only 2 or 3 args is accepted atm

functionSignature :: String -> Word32
functionSignature funDecl = read $ "0x" ++ take 8 (keccak256 funDecl)

eventSignature :: String -> Word256
eventSignature eventDecl = hexString2w256 $ "0x" ++ keccak256 eventDecl

-- Once the values have been placed in storage, the CODECOPY opcode should
-- probably be called.
constructor :: IntermediateContract -> [EvmOpcode]
constructor (IntermediateContract parties tcs _ _ _) =
  checkNoValue "Constructor_Header"
    ++ setExecutedWord tcs
    ++ saveParties parties

-- Checks that no value (ether) is sent when executing contract method
-- Used in both contract header and in constructor
checkNoValue :: String -> [EvmOpcode]
checkNoValue target =
  [ CALLVALUE
  , ISZERO
  , JUMPITO target
  , push 0
  , push 0
  , REVERT
  , JUMPDESTFROM target
  ]

-- Stores timestamp of creation of contract in storage
saveTimestampToStorage :: [EvmOpcode]
saveTimestampToStorage =
  [TIMESTAMP, push $ storageAddress CreationTimestamp, SSTORE]

-- Given a number of transfercalls, set executed word in storage
-- A setMemExpWord is not needed since that word is initialized to zero automatically
setExecutedWord :: [TransferCall] -> [EvmOpcode]
setExecutedWord [] = []
setExecutedWord tcs =
  [push $ 2 ^ length tcs - 1, push $ storageAddress Executed, SSTORE]


saveParties :: [Party] -> [EvmOpcode]
saveParties parties = savePartiesH $ zip [toInteger 0 ..] parties

savePartiesH :: [(PartyIndex, Party)] -> [EvmOpcode]
savePartiesH ((partyIndex, p) : parties) =
  savePartyToStorage partyIndex p ++ savePartiesH parties
savePartiesH _ = []

savePartyToStorage :: PartyIndex -> Party -> [EvmOpcode]
savePartyToStorage partyIndex (Bound address) =
  saveToStorage (storageAddress PartyMap) partyIndex (address2w256 address)
savePartyToStorage partyIndex (Free partyIdentifier) = saveToStorage
  (storageAddress PartyFreeMap)
  partyIdentifier
  (integer2w256 partyIndex)

getPartyFromStorage :: PartyIndex -> [EvmOpcode]
getPartyFromStorage partyIndex =
  [push partyIndex] ++ (getFromStorageStack $ storageAddress PartyMap)

saveToStorage :: Integer -> Integer -> Word256 -> [EvmOpcode]
saveToStorage prefix key value =
  [PUSH32 value] ++ [push key] ++ getStorageHashKeyStack prefix ++ [SSTORE]

getFromStorageStack :: Integer -> [EvmOpcode]
getFromStorageStack prefix = getStorageHashKeyStack prefix ++ [SLOAD]

getStorageHashKeyStack :: Integer -> [EvmOpcode]
getStorageHashKeyStack prefix =
  [ push 8
  , SHL
  , push prefix
  , OR
  , push freeSpaceOffset
  , MSTORE
  , push 0x32
  , push freeSpaceOffset
  , SHA3
  ]
  where
    -- TODO: Proper memory handling in state monad, instead of guessing free space
        freeSpaceOffset = 0x2000


-- Returns the code needed to transfer code from *init* to I_b in the EVM
-- 22 is the length of itself, right now we are just saving in mem0
-- TODO: Change '22' to a calculated length.
codecopy :: [EvmOpcode] -> [EvmOpcode] -> [EvmOpcode]
codecopy con exe =
  [ PUSH4 $ fromInteger (sizeOfOpcodes exe) -- DO NOT REPLACE THIS WITH A push :: Integer -> EvmOpcode!
  , PUSH4 $ fromInteger (sizeOfOpcodes con + 22) -- this may be the problem! -- DO NOT REPLACE THIS WITH A push :: Integer -> EvmOpcode!
  , push 0
  , CODECOPY
  , PUSH4 $ fromInteger (sizeOfOpcodes exe) -- DO NOT REPLACE THIS WITH A push :: Integer -> EvmOpcode!
  , push 0
  , RETURN
  , STOP
  ]

-- This does not allow for multiple calls.
jumpTable :: [EvmOpcode]
jumpTable =
  checkNoValue "Contract_Header"
    ++ [ push 0
       , CALLDATALOAD
       , PUSH32 (0xffffffff, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0)
       , AND
       , DUP1
       , DUP1
       , PUSH32
         (functionSignature "execute()", 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0)
       , EVM_EQ
       , JUMPITO "execute_method"
       , PUSH32
         (functionSignature "activate()", 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0)
       , EVM_EQ
       , JUMPITO "activate_method"
       , PUSH32
         (functionSignature "take(uint256)", 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0)
       , EVM_EQ
       , JUMPITO "take_method"
       , JUMPDESTFROM "global_throw"
       , push 0
       , push 0
       , REVERT
       ]

subroutines :: [EvmOpcode]
subroutines = transferSubroutine ++ transferFromSubroutine
 where
  transferFromSubroutine =
    funStartTF
      ++ storeFunctionSignature TransferFrom
      ++ storeArgumentsTF -- transferFrom(_from, _to, _amount) = transferFrom(party, self, amount)
      ++ pushOutSize
      ++ pushOutOffset
      ++ pushInSizeTF
      ++ pushInOffset
      ++ pushValue
      ++ pushCalleeAddress
      ++ pushGasAmount
      ++ callInstruction
      ++ checkExitCode
      ++ removeExtraArg
      ++ getReturnValueFromMemory
      ++ funEnd

  transferSubroutine =
    funStartT
      ++ storeFunctionSignature Transfer
      ++ storeArgumentsT -- transfer(_to, _amount) = transfer(party, amount)
      ++ pushOutSize
      ++ pushOutOffset
      ++ pushInSizeT
      ++ pushInOffset
      ++ pushValue
      ++ pushCalleeAddress
      ++ pushGasAmount
      ++ callInstruction
      ++ checkExitCode
      ++ removeExtraArg
      ++ getReturnValueFromMemory
      ++ funEnd

  funStartT  = [FUNSTART "transfer_subroutine" 3]
  funStartTF = [FUNSTART "transferFrom_subroutine" 3]

  storeFunctionSignature :: FunctionSignature -> [EvmOpcode]
  storeFunctionSignature Transfer =
    [ PUSH32
      ( functionSignature "transfer(address,uint256)"
      , 0x0
      , 0x0
      , 0x0
      , 0x0
      , 0x0
      , 0x0
      , 0x0
      )
    , push 0 -- TODO: We always use 0 here, since we don't use memory other places. Use monad to keep track of memory usage.
    , MSTORE
    ]
  storeFunctionSignature TransferFrom =
    [ PUSH32
      ( functionSignature "transferFrom(address,address,uint256)"
      , 0x0
      , 0x0
      , 0x0
      , 0x0
      , 0x0
      , 0x0
      , 0x0
      )
    , push 0
    , MSTORE
    ]
  -- TODO: Missing 'Get' case.

  storeArgumentsT =
    [ push 0x04
    , MSTORE -- store recipient (_to) in mem
    , push 0x24
    , MSTORE -- store amount in mem
    ]

  storeArgumentsTF =
    [ push 0x04
    , MSTORE -- store sender (_from) in mem
    , ADDRESS
    , push 0x24
    , MSTORE -- store own address (_to) in mem (recipient of transferFrom transaction)
    , push 0x44
    , MSTORE -- store amount in mem
    ]
  pushOutSize              = [push 0x20]
  pushOutOffset            = [push 0x0]
  pushInSizeTF             = [push 0x64]
  pushInSizeT              = [push 0x44]
  pushInOffset             = [push 0x0]
  pushValue                = [push 0x0]
  pushCalleeAddress        = [DUP6]
  pushGasAmount            = [push 0x32, GAS, SUB]
  callInstruction          = [CALL]
  checkExitCode            = [ISZERO, JUMPITO "global_throw"]
  removeExtraArg           = [POP]
  getReturnValueFromMemory = [push 0x0, MLOAD]
  funEnd                   = [FUNRETURN]

-- When calling execute(), PC must be set here
-- to check if the DC is activated
-- throw iff activated bit is zero
activateCheck :: [EvmOpcode]
activateCheck =
  [ JUMPDESTFROM "execute_method"
  , push $ storageAddress Activated
  , SLOAD
  , push 1
  , AND
  , ISZERO
  , JUMPITO "global_throw"
  ]

executeBody :: Compiler [EvmOpcode]
executeBody = do
  memExpCode <- concatMap executeMemExp . memExps . ceContract <$> get
  mrm        <- marginRefundMap . ceContract <$> get
  let marginRefundCode =
        evalState (concatMapM executeMarginRefundM (Map.assocs mrm)) 0
  transferCallCode <- executeTransferCalls
  return (memExpCode ++ marginRefundCode ++ transferCallCode)

-- This sets the relevant bits in the memory expression word in storage
-- Here the IMemExp should be evaluated. But only iff it is NOT true atm.
-- And also only iff current time is less than time in the IMemExp

-- The new plan is to use two bits to set the value of the memExp:
-- one if the memExp is evaluated to true, and one for false:
-- The empty value 00 would then indicate that the value of this
-- memExp has not yet been determined. The value 11 would be an invalid
-- value, 01 would be false, and 10 true.
executeMemExp :: IMemExp -> [EvmOpcode]
executeMemExp (IMemExp beginTime endTime count exp) =
  let checkIfExpShouldBeEvaluated =
          let
            -- It should be considered which of the next three codeblocks
            -- it is cheaper to put first. Both read from storage so it might
            -- be irrelevant.
              checkIfMemExpIsTrueOrFalse =
                  [ push $ storageAddress MemoryExpressionRefs
                  , SLOAD
                  , push $ 0x3 * 2 ^ (2 * count) -- bitmask
                  , AND
                  , JUMPITO $ "memExp_end" ++ show count
                  ]

              -- TODO: The same value is read from storage twice. Use DUP instead?
              checkIfTimeHasStarted =
                  [ push $ storageAddress CreationTimestamp
                  , SLOAD
                  , TIMESTAMP
                  , SUB
                  , push beginTime
                  , EVM_GT
                  , JUMPITO $ "memExp_end" ++ show count
                  ]

              -- If the memory expression is neither true nor false
              -- and the time has run out, its value is set to false.
              checkIfTimeHasPassed =
                  [ push $ storageAddress CreationTimestamp
                  , SLOAD
                  , TIMESTAMP
                  , SUB
                  , push endTime
                  , EVM_GT
                  , JUMPITO $ "memExp_evaluate" ++ show count
                  ]

              setToFalse =
                  [ push $ storageAddress MemoryExpressionRefs
                  , SLOAD
                  , push $ 2 ^ (2 * count) -- bitmask
                  , XOR
                  , push $ storageAddress MemoryExpressionRefs
                  , SSTORE
                  , JUMPTO $ "memExp_end" ++ show count
                  ]
          in  checkIfMemExpIsTrueOrFalse
                ++ checkIfTimeHasStarted
                ++ checkIfTimeHasPassed
                ++ setToFalse

      jumpDestEvaluateExp = [JUMPDESTFROM $ "memExp_evaluate" ++ show count]
      evaluateExpression =
        runExprCompiler (initialEnv { transferCallCount = count }) exp

       -- eval to false but time not run out: don't set memdibit
      checkEvalResult = [ISZERO, JUMPITO $ "memExp_end" ++ show count]

      setToTrue =
          [ push $ storageAddress MemoryExpressionRefs
          , SLOAD
          , push $ 2 ^ (2 * count + 1) -- bitmask
          , XOR
          , push $ storageAddress MemoryExpressionRefs
          , SSTORE
          ]
  in  checkIfExpShouldBeEvaluated
        ++ jumpDestEvaluateExp
        ++ evaluateExpression
        ++ checkEvalResult
        ++ setToTrue
        ++ [JUMPDESTFROM $ "memExp_end" ++ show count]

-- Return the code to handle the margin refund as a result of dead
-- branches due to evaluation of memory expressions
-- Happens within a state monad since each element needs an index to
-- identify it in storage s.t. its state can be stored

type MrId = Integer
type MarginCompiler a = State MrId a

newMrId :: MarginCompiler MrId
newMrId = get <* modify (+ 1)

-- This method should compare the bits set in the MemoryExpression word
-- in storage with the path which is the key of the element with which it
-- is called.
-- If we are smart here, we set the entire w32 (256 bit value) to represent
-- a path and load the word and XOR it with what was found in storage
-- This word can be set at compile time
executeMarginRefundM :: MarginRefundMapElement -> MarginCompiler [EvmOpcode]
executeMarginRefundM (path, refunds) = do
  i <- newMrId
  return $ concat
    [ checkIfMarginHasAlreadyBeenRefunded i
    , checkIfPathIsChosen path i
    , payBackMargin refunds
    , setMarginRefundBit i
    , [JUMPDESTFROM $ "mr_end" ++ show i]
    ]
 where
    -- Skip the rest of the call if the margin has already been repaid
  checkIfMarginHasAlreadyBeenRefunded i =
    [ push $ 2 ^ (i + 1) -- add 1 since right-most bit is used to indicate an active DC
    , push $ storageAddress Activated
    , SLOAD
    , AND
    , JUMPITO $ "mr_end" ++ show i
    ]
  -- leaves 1 or 0 on top of stack to show if path is chosen
  checkIfPathIsChosen mrme i =
    [ push $ path2Bitmask mrme
    , push $ storageAddress MemoryExpressionRefs
    , SLOAD
    , push $ otherBitMask mrme
    , AND
    , XOR
    , JUMPITO $ "mr_end" ++ show i -- iff non-zero refund; if 0, refund
    ]
  payBackMargin [] = []
  payBackMargin ((tokenAddr, recipient, amount) : ls) = -- push args, call transfer, check ret val
    getPartyFromStorage recipient -- TODO: This hould be PUSH20, not PUSH32. Will save gas.
      ++ [ PUSH32 $ address2w256 tokenAddr
         , push amount
         , FUNCALL "transfer_subroutine"
         , ISZERO
         , JUMPITO "global_throw"
         ]
      ++ payBackMargin ls

  setMarginRefundBit i =
    [ push $ 2 ^ (i + 1)
    , push $ storageAddress Activated
    , SLOAD
    , XOR
    , push $ storageAddress Activated
    , SSTORE
    ]

-- Ensures that only the bits relevant for this path are checked
otherBitMask :: [(Integer, Bool)] -> Integer
otherBitMask []                  = 0
otherBitMask ((i, _branch) : ls) = 3 * 2 ^ (i * 2) + otherBitMask ls

path2Bitmask :: [(Integer, Bool)] -> Integer
path2Bitmask [] = 0
path2Bitmask ((i, branch) : ls) =
  2 ^ (2 * i + if branch then 1 else 0) + path2Bitmask ls

-- Return highest index value in path, assumes the path is an ordered, asc list. So returns int of last elem
-- TODO: Rewrite this using better language constructs
path2highestIndexValue :: [(Integer, Bool)] -> Integer
path2highestIndexValue []                   = 0
path2highestIndexValue [(i, _branch)      ] = i
path2highestIndexValue ((_i, _branch) : ls) = path2highestIndexValue ls

-- Returns the code for executing all tcalls that function gets
executeTransferCalls :: Compiler [EvmOpcode]
executeTransferCalls = do
  opcodes <- loop 0 =<< gets (transferCalls . ceContract)

  -- Prevent selfdestruct from running after each call
  return $ opcodes ++ [STOP] ++ selfdestruct

 where
  loop :: Integer -> [TransferCall] -> Compiler [EvmOpcode]
  loop _ []         = return []
  loop i (tc : tcs) = do
    opcodes1 <- executeTransferCallsHH tc i
    opcodes2 <- loop (i + 1) tcs
    return (opcodes1 ++ opcodes2)

  selfdestruct = [JUMPDESTFROM "selfdestruct", CALLER, SELFDESTRUCT, STOP]

-- Return a new, unique label. Argument can be anything but should be
-- descriptive since this will ease debugging.
newLabel :: String -> Compiler String
newLabel desc = do
  compileEnv <- get
  let i = labelCount compileEnv
  let j = transferCallCount compileEnv
  let k = labelString compileEnv
  put compileEnv { labelCount = i + 1 }
  return $ desc ++ "_" ++ show i ++ "_" ++ show j ++ "_" ++ show k

-- Compile intermediate expression into EVM opcodes
-- THIS IS THE ONLY PLACE IN THE COMPILER WHERE EXPRESSION ARE HANDLED

compileExp :: Expr -> Compiler [EvmOpcode]
compileExp e = case e of
  Lit lit -> do
    mo    <- gets memOffset
    label <- newLabel "observable"
    return $ compileLit lit mo label

  -- MinExp and MaxExp can also be written without jumps: x^((x^y)&-(x<y))
  -- which is cheaper?

  MinExp e1 e2 -> compileExp e1 <++> compileExp e2 <++> do
    label <- newLabel "min_is_e1"
    return [DUP2, DUP2, EVM_GT, JUMPITO label, SWAP1, JUMPDESTFROM label, POP]

  MaxExp e1 e2 -> compileExp e1 <++> compileExp e2 <++> do
    label <- newLabel "max_is_e1"
    return [DUP2, DUP2, EVM_LT, JUMPITO label, SWAP1, JUMPDESTFROM label, POP]

  MultExp e1 e2 -> compileExp e1 <++> compileExp e2 <++> return [MUL]
  DiviExp e1 e2 -> compileExp e2 <++> compileExp e1 <++> return [DIV]
  AddiExp e1 e2 -> compileExp e1 <++> compileExp e2 <++> return [ADD]
  SubtExp e1 e2 -> compileExp e2 <++> compileExp e1 <++> return [SUB]
  EqExp   e1 e2 -> compileExp e1 <++> compileExp e2 <++> return [EVM_EQ]
  LtExp   e1 e2 -> compileExp e2 <++> compileExp e1 <++> return [EVM_LT]
  GtExp   e1 e2 -> compileExp e2 <++> compileExp e1 <++> return [EVM_GT]
  GtOrEqExp e1 e2 ->
    compileExp e2 <++> compileExp e1 <++> return [EVM_LT, ISZERO]
  LtOrEqExp e1 e2 ->
    compileExp e2 <++> compileExp e1 <++> return [EVM_GT, ISZERO]
  NotExp e1      -> compileExp e1 <++> return [ISZERO]
  AndExp e1 e2   -> compileExp e1 <++> compileExp e2 <++> return [AND]
  OrExp  e1 e2   -> compileExp e1 <++> compileExp e2 <++> return [OR]
  IfExp e1 e2 e3 -> do
    code1    <- compileExp e1
    code2    <- compileExp e2
    code3    <- compileExp e3
    ifLabel  <- newLabel "if"
    endLabel <- newLabel "end_if_else_exp"
    return
      $  code1
      ++ [JUMPITO ifLabel]
      ++ code3
      ++ [JUMPTO endLabel]
      ++ [JUMPDESTFROM ifLabel]
      ++ code2
      ++ [JUMPDESTFROM endLabel]

compileLit :: Literal -> Integer -> String -> [EvmOpcode]
compileLit lit mo _label = case lit of
  IntVal  i -> [push i]
  BoolVal b -> [push (if b then 0x1 else 0x0)] -- 0x1 is true
  Observable _ address key ->
    let functionCall = getFunctionCallEvm address
                                          (functionSignature "get(bytes32)")
                                          [Word256 (string2w256 key)]
                                          (fromInteger mo)
                                          (fromInteger mo)
                                          0x20
        moveResToStack = [push $ fromInteger mo, MLOAD]
    in  functionCall ++ moveResToStack

executeTransferCallsHH :: TransferCall -> Integer -> Compiler [EvmOpcode]
executeTransferCallsHH tc transferCounter = do
  mes <- gets (memExps . ceContract)
  let checkIfCallShouldBeMade =
        let checkIfTimeHasPassed =
                [ push $ storageAddress CreationTimestamp
                , SLOAD
                , TIMESTAMP
                , SUB
                , push $ delay tc
                , EVM_GT
                , JUMPITO $ "method_end" ++ show transferCounter
                ]

            -- Skip tcall if method has been executed already
            -- This only works for less than 2^8 transfer calls
            checkIfTCHasBeenExecuted =
                [ push $ storageAddress Executed
                , SLOAD
                , push $ fromInteger transferCounter
                , push 0x2
                , EXP
                , AND
                , ISZERO
                , JUMPITO $ "method_end" ++ show transferCounter
                ]

                -- This code can be represented with the following C-like code:
                -- if (memdibit == 00b) { GOTO YIELD } // Don't execute and don't set executed bit to zero.
                -- if (memdibit == 10b && !branch || memdibit == 01b && branch ) { GOTO SKIP } // TC should not execute. Update executed bit
                -- if (memdibit == 10b && branch || memdibit == 01b && !branch ) { GOTO PASS } // Check next memExp. If all PASS, then execute.
                -- TODO: the three above code blocks should be placed in an order which optimizes the gas cost over some ensemble of contracts
                -- Obviously, 3! possible orders exist.
            checkIfTcIsInActiveBranches = concatMap checkIfTcIsInActiveBranch
               where
                checkIfTcIsInActiveBranch (memExpId, branch) =
                  let yieldStatement =
                          [ push $ storageAddress MemoryExpressionRefs
                          , SLOAD
                          , DUP1 -- should be popped in all cases to keep the stack clean
                        -- TODO: WARNING: ATM this value is not being popped!
                          , push $ 0x3 * 2 ^ (2 * memExpId) -- bitmask
                          , AND
                          , ISZERO
                          , JUMPITO $ "method_end" ++ show transferCounter
                          ] -- GOTO YIELD
                      passAndSkipStatement =
                          [ push $ 2 ^ (2 * memExpId + if branch then 1 else 0) -- bitmask
                          , AND
                          , ISZERO
                          , JUMPITO $ "tc_SKIP" ++ show transferCounter
                          ]
                          -- The fall-through case represents the "PASS" case.
                  in  yieldStatement ++ passAndSkipStatement
        in  checkIfTimeHasPassed
              ++ checkIfTCHasBeenExecuted
              ++ checkIfTcIsInActiveBranches (memExpPath tc)

      callTransferToTcRecipient =
        let exprEnv = initialEnv { transferCallCount = transferCounter
                                 , memOffset         = 0x44
                                 , labelString       = "amount_exp"
                                 }
        in runExprCompiler exprEnv (amount tc)
          ++ [ push (maxAmount tc)
             , DUP2
             , DUP2
             , EVM_GT
             , JUMPITO $ "use_exp_res" ++ show transferCounter
             , SWAP1
             , JUMPDESTFROM $ "use_exp_res" ++ show transferCounter
             , POP
             ]

          ++ getPartyFromStorage (to tc)
          ++ [ PUSH32 $ address2w256 (tokenAddress tc)
             , DUP3
             , FUNCALL "transfer_subroutine"
             , ISZERO
             , JUMPITO "global_throw"
             ]

      checkIfTransferToTcSenderShouldBeMade =
        [ push (maxAmount tc)
        , SUB
        , DUP1
        , push 0x0
        , EVM_EQ
        , JUMPITO $ "skip_call_to_sender" ++ show transferCounter
        ]
        -- TODO: Here, we should call transfer to the
        -- TC originator (transfer back unspent margin)
        -- but we do not want to recalculate the amount
        -- so we should locate the amount on the stack.
        -- And make sure it is preserved on the stack
        -- for the next call to transfer.

      callTransferToTcOriginator =
        getPartyFromStorage (from tc)
          ++ [ PUSH32 $ address2w256 (tokenAddress tc)
             , DUP3
             , FUNCALL "transfer_subroutine"
             ]
          ++ [ISZERO, JUMPITO "global_throw"] -- check ret val

      -- Flip correct bit from one to zero and call selfdestruct if all tcalls compl.
      skipCallToTcSenderJumpDest =
        [JUMPDESTFROM $ "skip_call_to_sender" ++ show transferCounter, POP] -- pop return amount from stack

      updateExecutedWord =
        [ JUMPDESTFROM $ "tc_SKIP" ++ show transferCounter
        , push $ storageAddress Executed
        , SLOAD
        , push $ fromInteger transferCounter
        , push 0x2
        , EXP
        , XOR
        , DUP1
        , ISZERO
        , JUMPITO "selfdestruct"
        , push $ storageAddress Executed
        , SSTORE
        ]

      functionEndLabel = [JUMPDESTFROM $ "method_end" ++ show transferCounter]

  return
    $  checkIfCallShouldBeMade
    ++ callTransferToTcRecipient
    ++ checkIfTransferToTcSenderShouldBeMade
    ++ callTransferToTcOriginator
    ++ skipCallToTcSenderJumpDest
    ++ updateExecutedWord
    ++ functionEndLabel

-- This might have to take place within the state monad to get unique labels for each TransferFrom call
-- TODO: Add unique labels.
activateBody :: Compiler [EvmOpcode]
activateBody = do
  am <- gets (activateMap . ceContract)
  return
    $  [JUMPDESTFROM "activate_method"]
    ++ concatMap activateMapElementToTransferFromCall (Map.assocs am)
    -- set activate bit to 0x01 (true)
    ++ [push 0x01, push $ storageAddress Activated, SSTORE]
    ++ saveTimestampToStorage
    -- emit activated event
    ++ emitEvent
    ++ [STOP]
 where
  emitEvent = [PUSH32 $ eventSignature "Activated()", push 0, push 0, LOG1]

activateMapElementToTransferFromCall :: ActivateMapElement -> [EvmOpcode]
activateMapElementToTransferFromCall ((tokenAddress, partyIndex), amount) =
  pushArgsToStack ++ subroutineCall ++ throwIfReturnFalse
 where
  pushArgsToStack =
    getPartyFromStorage partyIndex
      ++ [PUSH32 $ address2w256 tokenAddress, push amount]
  subroutineCall     = [FUNCALL "transferFrom_subroutine"]
  throwIfReturnFalse = [ISZERO, JUMPITO "global_throw"]

takeBody :: Compiler [EvmOpcode]
takeBody = do
  return
    $  [JUMPDESTFROM "take_method"]
    ++ notActivatedCheck
    ++ loadPartyName
    ++ loadPartyIndex
    ++ checkAddress
    ++ saveAddress
    ++ [STOP]
 where
  notActivatedCheck =
    [push $ storageAddress Activated, SLOAD, JUMPITO "global_throw"]
  loadPartyName =
    [ push 0x4 -- Skip the method signature
    , CALLDATALOAD
    ]

  loadPartyIndex =
    (getFromStorageStack $ storageAddress PartyFreeMap) ++ [DUP1]
  checkAddress =
    (getFromStorageStack $ storageAddress PartyMap)
      ++ [push 0, EVM_LT, JUMPITO "global_throw"]
  saveAddress =
    [CALLER, SWAP1]
      ++ (getStorageHashKeyStack $ storageAddress PartyMap)
      ++ [SSTORE]

getMemExpById :: MemExpId -> [IMemExp] -> IMemExp
getMemExpById memExpId [] =
  error $ "Could not find IMemExp with ID " ++ show memExpId
getMemExpById memExpId (me : mes) =
  if memExpId == _IMemExpIdent me then me else getMemExpById memExpId mes

-- We also need to add a check whether the transferFrom function call
-- returns true or false. Only of all function calls return true, should
-- the activated bit be set. This bit has not yet been reserved in
-- memory/defined.

-- Given a list of things, t a, and a monadic function we map across that
-- returns a list of values inside a monad (e.g. Compiler [EvmOpcode]),
-- concatenate those result lists inside that monad.
concatMapM :: (Traversable t, Monad m) => (a -> m [b]) -> t a -> m [b]
concatMapM f = fmap concat . mapM f

(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
(<++>) xs ys = (++) <$> xs <*> ys

keccak256 :: String -> String
keccak256 fname =
  let keccak256H :: ByteString -> Digest Keccak_256
      keccak256H = hash
  in  show $ keccak256H $ pack fname

keccak256B :: ByteString -> String
keccak256B str =
  let keccak256H :: ByteString -> Digest Keccak_256
      keccak256H = hash
  in  show $ keccak256H str

address2w256 :: Address -> Word256
address2w256 ('0' : 'x' : addr) =
  let
    address2w256H (h0 : h1 : h2 : h3 : h4 : h5 : h6 : h7 : h8 : h9 : h10 : h11 : h12 : h13 : h14 : h15 : h16 : h17 : h18 : h19 : h20 : h21 : h22 : h23 : h24 : h25 : h26 : h27 : h28 : h29 : h30 : h31 : h32 : h33 : h34 : h35 : h36 : h37 : h38 : h39 : [])
      = ( 0x0
        , 0x0
        , 0x0
        , read ("0x" ++ [h0, h1, h2, h3, h4, h5, h6, h7])
        , read ("0x" ++ [h8, h9, h10, h11, h12, h13, h14, h15])
        , read ("0x" ++ [h16, h17, h18, h19, h20, h21, h22, h23])
        , read ("0x" ++ [h24, h25, h26, h27, h28, h29, h30, h31])
        , read ("0x" ++ [h32, h33, h34, h35, h36, h37, h38, h39])
        )
    address2w256H _ = undefined
  in
    address2w256H addr
address2w256 _ = undefined

hexString2w256 :: String -> Word256
hexString2w256 ('0' : 'x' : addr) =
  let
    hexString2w256H (h0 : h1 : h2 : h3 : h4 : h5 : h6 : h7 : h8 : h9 : h10 : h11 : h12 : h13 : h14 : h15 : h16 : h17 : h18 : h19 : h20 : h21 : h22 : h23 : h24 : h25 : h26 : h27 : h28 : h29 : h30 : h31 : h32 : h33 : h34 : h35 : h36 : h37 : h38 : h39 : h40 : h41 : h42 : h43 : h44 : h45 : h46 : h47 : h48 : h49 : h50 : h51 : h52 : h53 : h54 : h55 : h56 : h57 : h58 : h59 : h60 : h61 : h62 : h63 : [])
      = ( read ("0x" ++ [h0, h1, h2, h3, h4, h5, h6, h7])
        , read ("0x" ++ [h8, h9, h10, h11, h12, h13, h14, h15])
        , read ("0x" ++ [h16, h17, h18, h19, h20, h21, h22, h23])
        , read ("0x" ++ [h24, h25, h26, h27, h28, h29, h30, h31])
        , read ("0x" ++ [h32, h33, h34, h35, h36, h37, h38, h39])
        , read ("0x" ++ [h40, h41, h42, h43, h44, h45, h46, h47])
        , read ("0x" ++ [h48, h49, h50, h51, h52, h53, h54, h55])
        , read ("0x" ++ [h56, h57, h58, h59, h60, h61, h62, h63])
        )
    hexString2w256H _ = undefined
  in
    hexString2w256H addr
hexString2w256 _ = undefined

integer2w256 :: Integer -> Word256
integer2w256 i =
  let w32r = 2 ^ 32
  in  ( fromInteger (i `quot` w32r ^ 7)
      , fromInteger (i `quot` w32r ^ 6)
      , fromInteger (i `quot` w32r ^ 5)
      , fromInteger (i `quot` w32r ^ 4)
      , fromInteger (i `quot` w32r ^ 3)
      , fromInteger (i `quot` w32r ^ 2)
      , fromInteger (i `quot` w32r ^ 1)
      , fromInteger (i `quot` w32r ^ 0)
      )

bool2w8 :: Bool -> Word8
bool2w8 b = if b then 0x1 else 0x0

-- Store string in ASCII format, with appending zeros
string2w256 :: String -> Word256
string2w256 str =
  let showHex' c = showHex c "" -- partial evaluation of showHex
      keyArg    = concatMap (showHex' . ord) str -- get it to hex repr as string
      formatted = "0x" ++ keyArg ++ (replicate (64 - 2 * (length str)) '0')
  in  hexString2w256 formatted

-- Return the code for a function call.
-- This function should be used when generating the code
-- for the transferFrom function call, to generate the code
-- that probes oracles, etc.
getFunctionCallEvm
  :: Address
  -> Word32
  -> [CallArgument]
  -> Word8
  -> Word8
  -> Word8
  -> [EvmOpcode]
getFunctionCallEvm calleeAddress funSig callArgs inMemOffset outMemOffset outSize
  = storeFunctionSignature
    ++ storeArguments
    ++ pushOutSize
    ++ pushOutOffset
    ++ pushInSize
    ++ pushInOffset
    ++ pushValue
    ++ pushCalleeAddress
    ++ pushGasAmount
    ++ callInstruction
    ++ checkReturnValue -- should this be here?
 where
  storeFunctionSignature =
    [ PUSH32 (funSig, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0)
    , PUSH1 inMemOffset
    , MSTORE
    ]
  pushOutSize       = [PUSH1 outSize]
  pushOutOffset     = [PUSH1 outMemOffset]
  pushInSize        = [PUSH1 (0x4 + 0x20 * (fromIntegral (length callArgs)))]
  pushInOffset      = [PUSH1 inMemOffset]
  pushValue         = [PUSH1 0x0]
  pushCalleeAddress = [PUSH32 $ address2w256 calleeAddress]
  pushGasAmount     = [PUSH1 0x32, GAS, SUB]
  callInstruction   = [CALL]
  checkReturnValue  = [ISZERO, JUMPITO "global_throw"] -- cancel entire execution if call was unsuccesfull
  storeArguments    = storeArgumentsH callArgs 0
   where
    storeArgumentsH [] _ = []
    storeArgumentsH (arg : args) counter =
      storeArgumentsHH arg ++ (storeArgumentsH args (counter + 1))
     where
      storeArgumentsHH :: CallArgument -> [EvmOpcode]
      storeArgumentsHH (Word256 w256) =
        [PUSH32 w256, PUSH1 (inMemOffset + 0x4 + counter * 0x20), MSTORE]
      storeArgumentsHH OwnAddress =
        [ADDRESS, PUSH1 (inMemOffset + 0x4 + counter * 0x20), MSTORE]
      storeArgumentsHH (RawEvm evmOpcodes) = evmOpcodes

ppEvm :: EvmOpcode -> String
ppEvm instruction = case instruction of
  STOP         -> "00"
  ADD          -> "01"
  MUL          -> "02"
  SUB          -> "03"
  DIV          -> "04"
  SDIV         -> "05"
  MOD          -> "06"
  SMOD         -> "07"
  ADDMOD       -> "08"
  MULMOD       -> "09"
  EXP          -> "0a"
  SIGNEXTEND   -> "0b"
  EVM_LT       -> "10"
  EVM_GT       -> "11"
  SLT          -> "12"
  SGT          -> "13"
  EVM_EQ       -> "14"
  ISZERO       -> "15"
  AND          -> "16"
  OR           -> "17"
  XOR          -> "18"
  NOT          -> "19"
  BYTE         -> "1a"
  SHL          -> "1b"
  SHA3         -> "20"
  ADDRESS      -> "30"
  BALANCE      -> "31"
  ORIGIN       -> "32"
  CALLER       -> "33"
  CALLVALUE    -> "34"
  CALLDATALOAD -> "35"
  CALLDATASIZE -> "36"
  CALLDATACOPY -> "37"
  CODESIZE     -> "38"
  CODECOPY     -> "39"
  GASPRICE     -> "3a"
  EXTCODESIZE  -> "3b"
  EXTCODECOPY  -> "3c"
  BLOCKHASH    -> "40"
  COINBASE     -> "41"
  TIMESTAMP    -> "42"
  NUMBER       -> "43"
  DIFFICULTY   -> "44"
  GASLIMIT     -> "45"
  POP          -> "50"
  MLOAD        -> "51"
  MSTORE       -> "52"
  MSTORES      -> "53"
  SLOAD        -> "54"
  SSTORE       -> "55"
  JUMP         -> "56"
  JUMPI        -> "57"
  PC           -> "58"
  MSIZE        -> "59"
  GAS          -> "5a"
  JUMPDEST     -> "5b"
  PUSH1 w8     -> "60" ++ printf "%02x" w8
  PUSH4 w32    -> "63" ++ printf "%08x" w32
  PUSH32 (w32_0, w32_1, w32_2, w32_3, w32_4, w32_5, w32_6, w32_7) ->
    "7f"
      ++ printf "%08x" w32_0
      ++ printf "%08x" w32_1
      ++ printf "%08x" w32_2
      ++ printf "%08x" w32_3
      ++ printf "%08x" w32_4
      ++ printf "%08x" w32_5
      ++ printf "%08x" w32_6
      ++ printf "%08x" w32_7
  PUSHN ws ->
    printf "%02x" (0x60 + length ws - 1 :: Int) ++ concatMap (printf "%02x") ws
  DUP1         -> "80"
  DUP2         -> "81"
  DUP3         -> "82"
  DUP4         -> "83"
  DUP5         -> "84"
  DUP6         -> "85"
  SWAP1        -> "90"
  SWAP2        -> "91"
  SWAP3        -> "92"
  LOG0         -> "a0"
  LOG1         -> "a1"
  LOG2         -> "a2"
  CREATE       -> "f0"
  CALL         -> "f1"
  CALLCODE     -> "f2"
  RETURN       -> "f3"
  DELEGATECALL -> "f4"
  SELFDESTRUCT -> "ff"
  THROW        -> "fe"
  REVERT       -> "fd"
  FUNSTART _ _ -> undefined
  FUNSTARTA _  -> undefined
  FUNCALL   _  -> undefined
  FUNRETURN    -> undefined
  JUMPTO  _    -> undefined
  JUMPITO _    -> undefined

push :: Integer -> EvmOpcode
push = PUSHN . words'
 where
  words' :: Integer -> [Word8]
  words' i | i < 256 = [fromIntegral i]
  words' i           = words' (i `div` 256) ++ [fromIntegral $ i `mod` 256]

-- Was: PUSH32 (0xffffffff, 0, 0, 0, 0, 0, 0, 0)
push4BigEnd :: Integer -> [EvmOpcode]
push4BigEnd i = [push i, push (0xe0), push 2, EXP, MUL]
