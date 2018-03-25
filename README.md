# cc-untyped
## A Haskell implementation of the untyped lambda calculus

### Status [WIP]

This is a work in progress.

### Motivation

I'm currently reading the book
[Types and Programming Languanges](https://www.cis.upenn.edu/~bcpierce/tapl/index.html "Types and Programming Languanges")
by Benjamin C. Pierce which introduces its readers to basic concepts of
programming languages and type systems. In the course of the book several
implementations of different programming languages are discussed by the author.
An introduction to the untyped lambda calculus aswell as an interpreter
implementation in [OCaml](https://ocaml.org/ "OCaml") are presented in the
first part of the book.

To improve my [Haskell](https://www.haskell.org/ "Haskell") knowledge, to have
a playground for experimenting with different reduction strategies and to solve
the exercises from the book I decided to port the implementation to Haskell.

### Language

The core calculus that we evaluate supports the following expression. Parens can
always be used to group expression.
```
e:  x       a variable
    e e     function application
    \x. e   lambda abstraction
```

Additionally to these expression some extensions are supported which will get
desugared into the core by the parser.
```
e:  let x = e1 in e2
```

You can use a set of predefined names as free variables in terms. These are
`a`, `b`, `c`, and `d`

#### Evaluation

There are three evaluators available using

- 1.) normal order *default
- 2.) call by name
- 3.) call by value

to reduce the expressions. The default evaluator uses the normal order strategy.

### Usage

You can either start a repl by invoking the executable without any args or pass
a list of files to execute them.

```
$ cc-untyped-exe [-h] files?
```

#### Build

This project uses [stack](http://haskellstack.org "The Haskell Tool Stack") to
install its dependencies and to build the interpretere binary. To build and
install simply run:
```
stack setup
stack build
stack install # or: stack exec cc-untyped-exe
```

### Resources

- [Types and Programming Languanges](https://www.cis.upenn.edu/~bcpierce/tapl/index.html "Types and Programming Languanges")
- [Demonstrating Lambda Calculus Reduction](https://www.itu.dk/people/sestoft/papers/sestoft-lamreduce.pdf)
- [Happy User Guide](https://www.haskell.org/happy/doc/happy.pdf)
- [Alex User Guide](https://www.haskell.org/alex/doc/alex.pdf)
- [Haskell](https://www.haskell.org/ "Haskell")
- [OCaml](https://ocaml.org/ "OCaml")
