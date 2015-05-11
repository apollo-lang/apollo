# Language Design

## Language Evolution

Our purpose with Apollo was fairly well-defined at the onset of the project --
we wanted it to be a convenient tool for functionally composing music. Apollo
was to be very simple and minimal, having only a small number of features with
which the user could build more complex programs. We purposely delayed making
very specific decisions in order to allow ourselves sufficient space for
molding and refactoring the language at later stages.

### Syntax

We wanted Apollo to be statically typed, and this made us consider different
patterns for the syntax of the language. In the end, we found the syntax of
languages like Scala and Swift to strike a good balance between expressivity
and aesthetics.

Perhaps the syntactic feature that changed the most was the declaration of
derived data types. When writing the first version of the language reference
manual, we decided to use Java-like constructors for these. Throughout the
course of the semester, however, this syntax was heavily simplified into a form
which we found to be very expressive.

### Features

At the beginning of the semester, we considered building Apollo around a `play`
function, which would play a note or chord at any point in the program. This
involved making use of either a global mutable list or dealing with IO
extensively. It later became evident that doing either would contradict the
purpose of a purely functional language.

Our type system also underwent several transformations as we tried to get rid
of unnecessary layers of complexity. The `Atom` data type, for instance,
ended up encapsulating notes, chords, and rests altogether. We found this to be
a feature very elegantly tied to Apollo's broader purpose of making music
composition minimal and formal.

Towards the end of the project, we added more features (e.g., typed lambda
expressions, closures, unnamed higher-order functions, etc.). Core features of
the language like primitive data types and arithmetic operators remained
constant throughout. 

## Compiler Tools and Components

We originally intended to develop Apollo as a compiled language that was translated
from Apollo to Haskell and then from Haskell to MIDI. As we drafted initial
versions of the front-end, we wanted a quick and convenient way to test our
results. We ended up writing a REPL (Read-Evaluate-Print-Loop) for Apollo. This
effectively removed the intermediate Apollo to Haskell translation removing
another layer of complexity. Keeping things simple was always one of our main
objectives.

Haskell, the implementation language, was the most crucial tool for building
Apollo. The constraints that Haskell imposes really forced us to think about
the design patterns we were using and made us refactor anything that would not
be highly modular and modifiable. Haskell modules, like HCodecs for working
with MIDI, were crucial for writing the backend of the compiler. Alex and
Happy, the Haskell equivalents of Lex and Yacc, were also very fun to use and
integrated beautifully with Haskell's elegant type system.

## Reference Manual and Compiler

We started working on the compiler front-end slightly before completing the first
draft of our reference manual. However, we tried not to adhere too closely to
the initial reference manual; we changed the language pretty liberally
throughout the course of the semester. This flexibility was great for crafting
Apollo into something that we liked. Granted, we had to heavily update the
reference manual after having a semi-final version of Apollo up and running.

