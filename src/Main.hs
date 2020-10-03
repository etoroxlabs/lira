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

{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Foldable (for_)
import           Data.List.Split (splitOn)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Options.Applicative
import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           System.FilePath.Posix ((</>))
import           System.IO (stderr)

import           Lira.Version (showVersion, version)
import qualified Lira.Backends.Evm as Evm

type OutputFile = (FilePath, Text)
type Backend = FilePath -> Text -> Either Text [OutputFile]

backends :: [(Text, Backend)]
backends = [ ("evm", Evm.compile) ]

data Args = Args
  { srcFile :: FilePath -- ^ The Lira contract file
  , outDir  :: FilePath -- ^ The directory in which to place translation output
  , backend :: Text     -- ^ The backend with which to perform translation
  } deriving (Show)

main :: IO ()
main = runArgsParser >>= argsHandler

argsHandler :: Args -> IO ()
argsHandler Args { backend = backend
                 , srcFile = srcFile
                 , outDir = outDir
                 } = do
  case lookup backend backends of
    Just compile -> do
      srcText <- Text.readFile srcFile
      case compile srcFile srcText of
        Left err -> meh err
        Right files -> for_ files $ \(outFile, outText) -> do
          let outFilePath = outDir </> outFile
          Text.hPutStrLn stderr ("Writing file " <> Text.pack outFilePath <> "...")
          Text.writeFile outFilePath outText

    Nothing -> meh $
      "Unknown backend '" <> backend <> "'. " <>
      "Known backends: " <> commas (map fst backends)

runArgsParser :: IO Args
runArgsParser = customExecParser (prefs showHelpOnError) argsParserInfo

argsParserInfo :: ParserInfo Args
argsParserInfo =
  info (helper <*> argsParser) . mconcat $
    [ fullDesc
    , header ("Lira compiler " <> showVersion version)
    , progDesc "Compiles Lira contracts"
    ]

argsParser :: Parser Args
argsParser = Args <$> srcFileParser <*> outDirParser <*> backendParser
  where
    srcFileParser = strArgument . mconcat $
      [ metavar "<file.lir>"
      ]

    outDirParser = strOption . mconcat $
      [ long "output"
      , short 'o'
      , metavar "DIRECTORY"
      , help "Output directory for compiled contract"
      , value "."
      , showDefault
      ]

    backendParser = strOption . mconcat $
      [ long "backend"
      , short 'b'
      , metavar "BACKEND"
      , help "The code generator to use"
      , value "evm"
      , showDefault
      ]

commas :: [Text] -> Text
commas = Text.intercalate ", "

meh :: Text -> IO ()
meh message = do
  Text.hPutStrLn stderr message
  exitFailure
