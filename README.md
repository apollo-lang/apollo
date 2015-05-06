Apollo [![Build Status](https://travis-ci.org/apollo-lang/apollo.svg?branch=master)](https://travis-ci.org/apollo-lang/apollo)
======

A programming language for algorithmic and musical composition.

Contributors:

 * [Benjamin Matthew Kogan](https://github.com/benkogan)
 * [Javier Llaca](https://github.com/javierllaca)
 * [Reza Nayebi](https://github.com/rezanayebi)
 * [Roberto Jose De Amorim](https://github.com/rjamorim)
 * [Souren Sarkis Papazian](https://github.com/SourenP)

Getting Started
---------------

Apollo requires [Haskell Platform][]. Once you have that installed, the
following series of commands will install all necessary dependencies, build
the compiler, and run the integration test suite in that order:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
$ make config
$ make
$ make test
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The `make config` command is used to set up a [sandbox][] for project dependencies. It is not strictly necessary, but it is highly recommended.

[haskell platform]: https://www.haskell.org/platform
[sandbox]: https://www.haskell.org/cabal/users-guide/installing-packages.html#developing-with-sandboxes

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
