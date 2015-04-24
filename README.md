Apollo [![Build Status](https://travis-ci.org/apollo-lang/apollo.svg?branch=master)](https://travis-ci.org/apollo-lang/apollo)
======


A programming language for algorithmic and musical composition.

Contributors:

 * Benjamin Matthew Kogan (bmk2130@columbia.edu)
 * Javier Llaca (jl3960@columbia.edu)
 * Reza Nayebi (rn2324@columbia.edu)
 * Roberto Jose De Amorim (rja2139@columbia.edu)
 * Souren Sarkis Papazian (ssp2155@columbia.edu)

Getting Started
---------------

Apollo requires [Haskell Platform][]. Once you have that installed, the
following series of commands will install all necessary dependencies, build
the compiler, and run the integration test suite in that order:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
$ cabal install --only-dependencies
$ cd src/
$ make
$ make test
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

[haskell platform]: https://www.haskell.org/platform

Usage
-----

*Note: the `apollo` CLI is still subject to change.*

By default, `apollo` will evaluate a program sent through `stdin`:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
$ echo "1 + 1" | ./apollo
2
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To parse a program's abstract syntax tree, use the `--ast` flag:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
$ echo "case (False) 1 otherwise 2" | ./apollo --ast
Program [StExp (Cond (ApolloBool False) (ApolloInt 1) (ApolloInt 2))]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To use Apollo's Read-Evaluate-Print Loop, use the `--repl` flag:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
$ ./apollo --repl
apollo> 1 + 1
2
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
