Development and run-time environment
------------------------------------

**Souren Papazian**

### Development Environment

Apollo was developed in the Unix environment. [Git][] was used for version control, while [GitHub][] was used for repository hosting, management, and code review. [Travis Ci][] was used for continuous integration and monitored via its integration with GitHub.

The interpreter itself is written in Haskell and targeted to GHC 7.8.3. [Cabal][] is used to create a sandboxed environment for dependencies, install required packages, and build the interpreter executable.

[git]: http://git-scm.com
[github]: https://github.com
[travis ci]: https://travis-ci.org
[cabal]: https://www.haskell.org/cabal/


### Makefile commands

#### `make config`

Installs all dependencies using the Cabal config. Cabal is a Haskell package-manager and build system. We used a Cabal sandbox to avoid any conflicts with existing Haskell packages and to install any missing or outdated packages. The sandbox essentially builds all packages in isolation much like Python’s `virtualenv`. This makes compiling the source code on different systems much simpler not only for the user, but also for ourselves during development.

#### `make`

Compiles the source code using `cabal build`. All the dependencies and their acceptable version ranges are stored in a file called "apollo.cabal." This also includes all the information needed to compile the Apollo interpreter such as dependencies, build tools, the compiler flags, and other metadata.

#### `make test`

Runs the integration test-suite. This target depends on the executable. If called with an outdated build of `apollo` --- or is missing it entirely --- the executable will be compiled first.

#### `make install`

This will copy the `apollo` executable into /usr/local/bin.

#### `make uninstall`

This will remove the apollo executable from /usr/local/bin.

*Makefile:*

```make
EXE     := apollo
BUILD   := ./dist/build/$(EXE)
SOURCES := $(wildcard src/*)
PREFIX  ?= /usr/local


.PHONY: all
all: $(EXE)

.PHONY: config
config:
    cabal sandbox init
    cabal install --only-dependencies

.PHONY: install
install: $(EXE)
    cp $(BUILD)/$(EXE) $(PREFIX)/bin/$(EXE)

.PHONY: uninstall
uninstall:
    rm -f $(PREFIX)/bin/$(EXE)

.PHONY: test
test: $(EXE)
    @./tests/run.sh

$(EXE): $(SOURCES)
    cabal build
    ln -s -f $(BUILD)/$(EXE) ./$(EXE)

```


*apollo.cabal:*

```cabal
name:                apollo
version:             0.0.0.0
category:            Language
build-type:          Simple
extra-source-files:  README.md
cabal-version:       >=1.10
homepage:            https://github.com/apollo-lang/apollo
license:             MIT
license-file:        LICENSE

synopsis:            A programming language for algorithmic and musical composition.

author:              Ben Kogan
                   , Javier Llaca
                   , Reza Nayebi
                   , Roberto Jose De Amorim
                   , Souren Papazian

executable apollo
  main-is:             Main.hs

  other-modules:       Check
                     , Env
                     , Error
                     , Eval
                     , Expr
                     , Lex
                     , Parse
                     , Util
                     , Midi
                     , Type

  build-depends:       base           >=4.6  && <4.8
                     , regex-posix    >=0.95 && <0.96
                     , mtl            >=2.1  && <2.2
                     , array          >=0.5  && <0.6
                     , transformers   >=0.3  && <0.4
                     , containers     >=0.5  && <0.6
                     , HCodecs        >=0.5  && <0.6

  other-extensions:    CPP
  hs-source-dirs:      src
  build-tools:         alex          >= 3.1  && <3.2
                     , happy         >= 1.19 && <1.20
  default-language:    Haskell2010
  ghc-options:         -Wall

```

Once all the source code is in place, these commands should compile `apollo` and run the test suite.

```
$ make config
$ cabal install happy alex
$ make test
```

If there are any global packages already installed on the user's machine, cabal will not reinstall them.

If you run into the following error message when you run `make config`:

```
cabal: The following packages are likely to be broken by the reinstalls: <packages omitted>
```

you may have globally-installed packages that conflict with Apollo's dependencies. Unregister the indicated global packages with the following command:

```
ghc-pkg unregister --global <package_name>
```

### Runtime Environment

The `apollo` executable provides several options:

```
$ apollo --help
Apollo: a language for algorithmic music composition

Usage: apollo [options|-] <source file> [-o <output>]

Options:
       --repl      Start Read-Evaluate-Print-Loop
       --ast       Print a program's abstract syntax tree
    -h|--help      Print this message
    -o <output>    Output midi to specified filename if source file present
       -           Read from stdin
```

`apollo --repl` will run the read-evaluate-print loop. This provides an environment for interactively evaluating single expressions.

`apollo --ast` prints the abstract syntax tree of an expression input through `stdin`.

You can pass a valid Apollo file into the interpreter with `apollo filename.ap`. If `main` is defined a midi file will be output. The `-o` option is used to specify the midi filename.

Since Apollo is implemented in Haskell as an interpreter, no intermediate source code is generated and all the memory allocation is dealt with by Haskell.

The midi file must be run with any external program. Apollo does not provide a midi player.
 
#### Prelude

Apollo includes a standard library called the prelude. The prelude is implemented in native Apollo code.

Before interpreting a program, Apollo interprets the contents of the prelude through the evaluation stage and discards the results. This populates the type environment and expression environment with the prelude's types and function definitions, respectively. These environments are then used in interpreting user code and so the prelude's contents are made available to the user.
