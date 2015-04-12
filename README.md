Apollo
======

A programming language for algorithmic and musical composition.

Contributors:

 * Benjamin Matthew Kogan (bmk2130@columbia.edu)
 * Javier Llaca (jl3960@columbia.edu)
 * Reza Nayebi (rn2324@columbia.edu)
 * Roberto Jose De Amorim (rja2139@columbia.edu)
 * Souren Sarkis Papazian (ssp2155@columbia.edu)

Building
--------

To build the `apollo` compiler, change to the `src/` directory and issue the command:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
$ make
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

To use the Apollo Read-Evaluate-Print Loop (REPL), use the `--repl` flag:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
$ ./apollo --repl
apollo> 1 + 1
2
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Tests
-----

To run all integration tests, change to the `src/` directory and issue the command:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
$ make test
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

