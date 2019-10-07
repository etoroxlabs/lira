<p align="center"><img width=30% src="https://github.com/etoroxlabs/lira/blob/master/docs/logo-image.svg"></p>
<p align="center"><img width=40% src="https://github.com/etoroxlabs/lira/blob/master/docs/logo-text.svg"></p>
<br/>
<br/>

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
![Haskell](https://img.shields.io/badge/haskell-lts--13.20-blue)
[![Build Status](https://img.shields.io/circleci/build/github/etoroxlabs/lira)](https://circleci.com/gh/etoroxlabs/lira)
![Dependencies](https://img.shields.io/discourse/https/community.lira.org/likes)
[![GitHub Issues](https://img.shields.io/github/issues/etoroxlabs/lira.svg)](https://github.com/etoroxlabs/lira/issues)
![Contributions welcome](https://img.shields.io/badge/contributions-welcome-orange.svg)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](https://opensource.org/licenses/MIT)

Lira is a declarative domain-specific language designed to be the backbone of
financial contracts that can be executed on the blockchain.

Lira aims at being simple and easy-to-follow allowing users 
to define simple yet highly complex financial contracts.
Lira is formally verified and based on internationally 
recognized academic research 
([Egelund-Müller B, Elsman M, Henglein F, Ross O (2017)](https://github.com/etoroxlabs/lira/blob/master/docs/ross.pdf)).

This repository provides a Lira compiler that you can start using today.
Currently, it only compiles to EVM but it can be extended 
with other backends as well.

# Table of Contents
<!--ts-->
  * [Try the Lira demo](#try-the-lira-demo---demoliraorg)
  * [The Lira Language](#the-lira-language)
    * [Definition](#definition)
    * [Examples](#examples)
      * [Example 1: Future](#example-1-future)
      * [Example 2: European put option, an Insurance Against a Drop in the ETH price](#example-2-european-put-option-an-insurance-against-a-drop-in-the-eth-price)
    * [Application Binary Interface (ABI) of the produced contracts](#application-binary-interface-abi-of-the-produced-contracts)
  * [Installation](#installation)
    * [Prerequisites](#prerequisites)
  * [Future developments](#future-developments)
<!--te-->

# Try the Lira demo - [demo.lira.org](https://demo.lira.org)

To show a possible integration of the language, we provide a graphical
frontend use-case for creating, deploying and monitoring future
contracts. Behind the scenes, this frontend will generate
corresponding Lira code which is subsequently compiled to EVM and
deployed to Ethereum. By generating Lira code, we
ensure that the static guarantees of the language apply regardless
of frontend functionality. Additionally, the frontend makes it
possible to view both the generated Lira code and the compiled EVM
bytecode.

Using a non-Turing complete DSL for this purpose has several
advantages. In particular, the language is restricted such that using
it to specify unintended behavior is impossible. Further, as the
semantics of the language are formally verified, contracts specified
in the language is guaranteed to behave as intended and to only have a
single interpretation.

# The Lira Language
Before we give the full definition of the language, let's go through a few of
the functions. 

To transfer a unit token from address `p1` to address `p2`, the `transfer(a, p1,
p2)` function is used, where `a` is the token constract address (e.g. eToroUSD).

Transferring an arbitrary token amount can be done by using `scale(n, e, c)`, where
`n` is the maximum amount of tokens the contract will lock in escrow, `e` is an
expression resolving in the actual amount of tokens to transfer and `c` is the
ERC-20 token contract. We need to lock tokens in escrow since the actual token
amount might be evaluated at runtime. The number of tokens to transfer can never
exceed the maximum amount.

By combining the transfer function
and the scale function above, we can transfer an arbitrary amount of tokens
using `scale(a, e, transfer(a, p1, p2))`.

If a contract should be executed at a specific time in the future,
`translate(t,c1)` can be used, where `t` is the time offset.

Notice how the contracts can be composed of simpler contracts by the
constructors `transfer`, `scale` and `translate` (and a few more defined below).

## Definition
Below is the full definition of the language written as a context-free grammar
definition of the language in which the derivative contracts are written:

```
contracts:
c ::= scale(n,e,c1) | zero | both(c1,c2) |
      transfer(a, p1, p2) | translate(t,c1) |
      if e within t1
      then c1 else c2

expressions:      
e ::= b | obs(ot, f, t) | e1 op e2 | uop e1

time:
t ::= now | u(n)

time unit:
u ::= seconds | minutes | hours | days | weeks

operators:
op ::= + | - | x | / | = | if | or | and | min | max
uop ::=  not

operator types:
ot ::= int | bool
```
where 

* n is a natural number
* p is a party to the contract identified by an Ethereum address
* a is a token contract address
* f is the address of a feed
* b is a whole number
* obs is an observable depending on external information

Addresses are written as `0x[0-9a-f]{40}`

## Examples
### Example 1: Future
One of the simpler useful examples is a future contract. 
The code below describes the contract, namely a legal agreement to buy or
sell something at a predetermined price at a specified time in the future,
between two parties not knowing each other. The contract holds a specific amount of tokens which both parties will receive in full amount at the maturity of the contract. The function `both<c1,c2>` is
executing both contracts `c1` and `c2`, in this case transferring the specified
currencies and the specified amount to each of the parties.
```
translate(
    seconds(<Time>),
    both(
        scale(
            <Upper limit>,
            <Amount>,
            transfer(
                <Currency>,
                <me>,
                <Counterparty>
            )
        ),
        scale(
            <Upper limit>,
            <Amount to receive>,
            transfer(
                <Currency to receive>,
                <Counterparty>,
                <me>
            )
        )
    )
)
```

### Example 2: European put option, an Insurance Against a Drop in the ETH price
Scenario: A owns 1 ETH and A would like insurance of a drop in the ETH price
below 100 USD three months from now. So A would like a contract whose value plus
the value of ether is at least 100 USD. This can be achieved by the following
contract:
```
translate(
    days(90),
    scale(
        100,
        max(0, 100 - obs(int, priceFeed, ETHUSD)),
        transfer(USDEX, B, A )
    )
)
```
If the ETH price at the strike time is 10 USD, then this contract will payout
90 USD, thus guaranteeing A a value of 100 USD at the maturity of the contract.
eToroUSD is the address of an ERC20-compliant token.

Note that the first parameter to the scale function (defining the token amount
to lock in escrow) is important when the second parameter (the token amount) can
depend on external information. Without the tokens in escrow, we would have no guarantee
that the counterparty would have the liquidity needed at execution time.

## Application Binary Interface (ABI) of the produced contracts
The Lira contract currently compiles into the Ethereum's ABI and has two
methods: `activate()` and `execute()`.
 * `activate()` collects the margin from the parties' accounts and starts the
   timer. Will only succeed if the parties have allowed the Lira contract to withdraw from their balance through the ERC20 contract call `approve`.
 * `execute()` checks whether any subparts of the contracts are ready to be paid
   out to the parties or any margins can be paid back.

 `activate()` and `execute()` may change state.

# Installation
At the moment the only way to compile Lira contracts is to build the compiler manually.
1. Build the compiler by running
```
$ stack install
```
2. You can now compile one of the examples by executing the following
```
$ mkdir build
$ lira -o build examples/BettingExample0.lir
```
Remember to change the placement addresses in the examples with real ones.

## Prerequisites
Haskell Tool Stack is required as the development environment. 
To install follow their [README](https://docs.haskellstack.org/en/stable/README/) 
or execute the following statement:
```
$ curl -sSL https://get.haskellstack.org/ | sh
```

# Future developments
Lira is still a work in progress, and could greatly benefit from help from the community.
The following points shows what we envision for Lira in the future.

* Work with the community to get a security audit for the compiler
* Enable fractional margin requirements into the contract language. 
  Currently, the contracts need to be fully collateralized. 
* Improve tooling by providing libraries that helps with easier startup
* Seek funding for further research to extend the functionality of the language
* Improve compiler documentation, both from the user's perspective and the developer's
