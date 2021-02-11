<p align="center"><img width="20%" src="https://github.com/etoroxlabs/lira/blob/master/docs/logo-image.svg"></p>
<p align="center"><img width="30%" src="https://github.com/etoroxlabs/lira/blob/master/docs/logo-text.svg"></p>
<br/>

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
![Haskell](https://img.shields.io/badge/haskell-lts--13.20-blue)
[![Build Status](https://img.shields.io/circleci/build/github/etoroxlabs/lira)](https://circleci.com/gh/etoroxlabs/lira)
[![GitHub Issues](https://img.shields.io/github/issues/etoroxlabs/lira.svg)](https://github.com/etoroxlabs/lira/issues)
![Contributions welcome](https://img.shields.io/badge/contributions-welcome-orange.svg)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](https://opensource.org/licenses/MIT)

# The Lira Language

Lira is a declarative domain-specific language designed for expressing
financial contracts that can be executed on the blockchain.

Lira aims at being simple and easy-to-follow allowing users with a financial
background, but without extensive programming experience to simply define
highly complex financial contracts.

Lira is based on internationally recognized academic research. The language
semantics are formally verified by [Bahr P., Berthold J., Elsman M.
(2015)][bahr15], and its execution on the Ethereum blockchain is covered by
[Egelund-Müller B., Elsman M., Henglein F., Ross O. (2017)][muller17]. The
present implementation was initiated by Værge, T., Gram, M. (2017) and is
credited to various authors; see [package.yaml][pyaml].

[bahr15]: https://bahr.io/pubs/entries/bahr15icfp.html
[muller17]: https://github.com/etoroxlabs/lira/blob/master/docs/ross.pdf
[pyaml]: ./package.yaml

This repository provides a Lira compiler that you can start using today.
Currently, it only compiles to EVM but it can be extended with other backends.

## Table of Contents
<!--ts-->
  * [The Lira Language](#the-lira-language)
    * [Try the Lira demo](#try-the-lira-demo---demoliraorg)
    * [Introduction](#introduction)
    * [Syntax](#syntax)
    * [More examples](#more-examples)
      * [Example 1: Future](#example-1-future)
      * [Example 2: European put option, an Insurance Against a Drop in the ETH price](#example-2-european-put-option-an-insurance-against-a-drop-in-the-eth-price)
    * [Application Binary Interface (ABI) of the produced contracts](#application-binary-interface-abi-of-the-produced-contracts)
    * [Installation](#installation)
      * [Prerequisites](#prerequisites)
    * [Future developments](#future-developments)
<!--te-->

## Try the Lira demo - [demo.lira.org](https://demo.lira.org)

To demonstrate one possible integration of the language, we provide a graphical
front-end use-case for creating, deploying and monitoring future contracts.
Behind the scenes, this front-end will generate corresponding Lira code which
is subsequently compiled to EVM and deployed to Ethereum. By generating Lira
code, we ensure that the static guarantees of the language apply regardless of
front-end functionality. Additionally, the front-end makes it possible to view
both the generated Lira code and the compiled EVM bytecode.

Using a domain-specific language that isn't Turing-complete has several
advantages. In particular, expressing unintended within the contract logic
is not possible. Further, as the semantics of the language are formally
verified, contracts specified in the language are guaranteed to behave as
intended and to only have a single interpretation.

## Introduction

Let's begin with some examples.

To transfer one unit of a tokenized asset, e.g. [`USDEX`][usdex], from Alice to Bob:

```
transfer(USDEX, Alice, Bob)
```

[usdex]: https://www.etorox.com/exchange/us-dollar/

To transfer some other amount of an asset, a contract can be scaled:

```
scale(10, 10, transfer(USDEX, Alice, Bob))
```

The first 10 is a constant that denotes the maximum amount of tokens that will
be locked in escrow before the contract is executed. The second 10 is an
expression calculated at run-time execution and represents the actual amount
of tokens to transfer.

The expression can be variable at run-time by depending on *observables*, but
it cannot exceed the maximum value known at compile-time.

This example can be extended with an observable:

```
scale(
  9000,
  obs(int, PriceFeed, BTCUSDEX),
  transfer(USDEX, Alice, Bob))
```

Here a quantity of `USDEX` tokens is transferred from Alice to Bob that depends
on an external price feed, `PriceFeed`, and a `BTCUSDEX` key presumed to show
the price of Bitcoin, but at most 9000. If the price, upon execution, is less,
the remainder will be sent back to Alice, and if the price is more, only 9000
`USDEX` is sent to Bob.

To perform two transfers in one contract:

```
both(
  scale(9000, obs(int, PriceFeed, BTCUSDEX), transfer(USDEX, Alice, Bob)),
  transfer(WBTC, Bob, Alice))
```

Here, upon execution, Alice sends Bob a quantity of `USDEX` that corresponds to
the price of one Bitcoin, but at most USD 9000, and Bob sends Alice one `WBTC`,
an ERC-20 wrapped Bitcoin.

A contract can be executed at some relative time offset in the future:

```
translate(
  days(30),
  transfer(WBTC, Bob, Alice))
```

This is relative to the time of contract activation.

Lastly, a contract can be conditional based on a predicate and a time frame.
The predicate may contain observables, and the time frame is also relative
to the time of contract activation. For example:

```
scale(10, 10,
  if obs(int, PriceFeed, BTCUSDEX) > 9000 within days(7)
    then transfer(USDEX, Alice, Bob)
    else transfer(USDEX, Bob, Alice))
```

Here Alice and Bob each bet 10 `USDEX` on whether the price of Bitcoin will go
above 9000 USD within 7 days of contract activation. A one-sided variation of
this bet that isn't active immediately after contract activation is shown here:

```
translate(
  days(7),
  if obs(int, PriceFeed, BTCUSDEX) > 9000 within days(7)
    then scale(10, 10, transfer(USDEX, Alice, Bob))
    else zero)
```

The price feed isn't checked until 7 days have passed, but at that point, upon
executing the contract, it will evaluate whether Alice should send Bob 10 `USDEX`
or not for an entire week, or until the condition is true. If not, the escrow
will be transferred back to Alice.

## Syntax

Below is a full description of Lira's syntax.

```
contract ::=
    'transfer(' asset ',' party ',' party ')'
  | 'scale(' max ',' expr ',' contract ')'
  | 'both(' contract ',' contract ')'
  | 'translate(' time ',' contract ')'
  | 'if' expr 'within' time 'then' contract 'else' contract
  | 'zero'

expr ::=
    'true'
  | 'false'
  | expr binop expr
  | 'max(' expr ',' expr ')'
  | 'min(' expr ',' expr ')'
  | unop expr
  | 'obs(' obsType ',' obsAddress ',' obsKey ')'
  | ['0'-'9']+

time ::= 'now' | ['0'-'9']+ timeUnit

timeUnit ::= 'seconds' | 'minutes' | 'hours' | 'days' | 'weeks'

binop ::= '+' | '-' | 'x' | '/' | '=' | 'or' | 'and'

unop ::= 'not'

obsType ::= 'int' | 'bool'

obsAddress ::= address

party ::= address

address ::= '0x' ['0'-'9', 'a'-'f']{40}
```

## More examples

### Example 1: Future

One of the simpler useful examples is a future contract.  The code below
describes the contract, namely a legal agreement to buy or sell something at a
predetermined price at a specified time in the future, between two parties not
knowing each other. The contract holds a specific amount of tokens which both
parties will receive in full amount at the maturity of the contract. The
function `both<c1,c2>` is executing both contracts `c1` and `c2`, in this case
transferring the specified currencies and the specified amount to each of the
parties.

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

Scenario: Alice owns 1 wrapped ETH and would like insurance of a drop in the
ETH price below 100 USD three months from now. So Alice wants a contract whose
value plus the value of ether is at least 100 USD. This can be achieved by the
following contract:

```
translate(
  days(90),
  scale(
    100,
    max(0, 100 - obs(int, PriceFeed, ETHUSDEX)),
    transfer(USDEX, Bob, Alice)))
```

If the ETH price at the strike time is 10 USD, then this contract will pay out
90 USDEX, thus guaranteeing Alice a value of 100 USD at the maturity of the
contract.

The first parameter to `scale()` (the token amount to lock in escrow) is
important when the second parameter (the token amount) depends on the external
price feed.  Without the tokens in escrow we would have no guarantee that the
counterparty has the asset at execution time.

## Application Binary Interface (ABI) of the produced contracts

The Lira contract currently compiles into the Ethereum's ABI and has two
methods: `activate()` and `execute()`.

 * `activate()` collects the margin from the parties' accounts and starts the
   timer. Will only succeed if the parties have allowed the Lira contract to
   withdraw from their balance through the ERC-20 contract call `approve`.
 * `execute()` checks whether any subparts of the contracts are ready to be
   paid out to the parties or any margins can be paid back.

 `activate()` and `execute()` may change the state of the deployed contract.

## Installation

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

### Prerequisites

Haskell Tool Stack is required as the development environment.
To install follow their [README](https://docs.haskellstack.org/en/stable/README/)
or execute the following statement:
```
$ curl -sSL https://get.haskellstack.org/ | sh
```

## Future developments

Lira is still a work in progress and could greatly benefit from help from the
community.  The following points shows what we envision for Lira in the future.

* Work with the community to get a security audit of the compiler
* Enable fractional margin requirements into the contract language.
  Currently, the contracts need to be fully collateralized.
* Improve tooling by providing libraries that helps with easier startup
* Seek funding for further research to extend the functionality of the language
* Improve compiler documentation, both from the user's perspective and the developer's
