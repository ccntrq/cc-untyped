# cc-untyped
## A Haskell implementation of the untyped lambda calculus
---

### Status [WIP]

This is a work in progress.

### Motivation

I'm currently reading the book
[Types and Programming Languanges](https://www.cis.upenn.edu/~bcpierce/tapl/index.html "Types and Programming Languanges")
by Benjamin C. Pierce which introduces its readers to basic concepts of
programming languages and type systems. In the course of the book several
implementations of different programming languages are discussed by the author.
A introduction to the untyped lambda calculus aswell as an interpreter
implementation in [OCaml](https://ocaml.org/ "OCaml") presented in the first
part of the book.

To improve my [Haskell](https://www.haskell.org/ "Haskell") knowledge, to have
a playground for experimenting with different reduction strategies and to solve
the exercises from the book I decided to port the implementation to Haskell.

### Implementation

I wanted to use [parsec](https://hackage.haskell.org/package/parsec) for the
lexer and parser as an easy entry point to learning and using the library.
Searching on the web I came across
[this](http://mattwetmore.me/posts/parsing-combinators-with-parser-combinators.html)
blog post which describes how to implement a parser with parsec for the
evaluator discussed in the book.

I used this parser and manually transpiled the OCaml evaluator to Haskell. The
evaluator from the book uses a *call by value* reduction strategy. To get a
better understanding of the different reduction strategies I read the paper
[Demonstrating Lambda Calculus Reduction](http://www.itu.dk/people/sestoft/papers/sestoft-lamreduce.pdf)
by Peter Sestoft.

### Usage

```
$ cc-untyped-exe
cc-untyped v0.1.0
Welcome to the cc-untyped repl!
Please enter a lambda term or type 'quit' to exit
λ:(\x.x x)(\x.x)
Reducing:
((λx.(x x)) (λx.x))
Reducing:
((λx.x) (λx.x))
Reducing:
(λx.x)
Done.
λ:
```

#### Build

This project uses [stack](http://haskellstack.org "The Haskell Tool Stack") to
install its dependencies and to build the interpretere binary. To build an
install simply run:
```
stack setup
stack build
stack install # or: stack exec cc-untyped-exe
```
