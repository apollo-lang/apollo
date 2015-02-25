Apollo Language Whitepaper
==========================

Team number 8. Contributors:

- **Tester & Validator:** Roberto Jose De Amorim (rja2139)
- **Language & Tools Guru:** Benjamin Matthew Kogan (bmk2130)
- **System Integrator:** Javier Llaca (jl3960)
- **Project Manager:** Reza Nayebi (rn2324)
- **System Integrator:** Souren Sarkis Papazian (ssp2155)

Implementation language: Haskell.

Introduction
------------

Apollo is a programming language for algorithmic and musical composition. The language provides an interface to leverage light-weight functional paradigms in order to produce a target program that generates a musical output when run. Apollo is intended to be usable by a programmer with knowledge of basic functional constructs and no prior experience with music creation. The fine-details of synthesizing music are abstracted into a familiar typed-interface. Instead of direct note-manipulation, common programming types like integers and characters are mapped to musical values when interpreted. In effect, Apollo thus allows the programmer to *hear the sound of an algorithm*.

Rationale
---------

There are a number of extant music programming languages such as [RTcmix][] and [SuperCollider][]. Apollo shares with these languages an interest in providing an abstraction for music creation to the programmer. Apollo differs from these in the depth and the intent of its abstractions. Virtually all common music programming languages provide a direct interface to sonic manipulation on the level of notes and frequencies. This allows the programmer to *act* as a musician by writing code.

Apollo, in contrast, abstracts sonic details in favor of simplicity. The specifics of musical articulation are eschewed. In the place of deep --- but complex --- sonic articulation, the programmer should be able to interpret familiar data-values using a `play` function or optionally act on a discrete note-type for greater control, if desired. Instead of conceiving of specific musical compositions prior to programming, the user conceives of a program that is transformed into music during execution. Apollo thus takes a more experimental approach where the programmer is able to *become* a musician through the act of programming.

[RTcmix]: http://rtcmix.org
[SuperCollider]: http://supercollider.sourceforge.net

Language Features
-----------------

- Simple
- Abstract
- Statically-typed
- Imperative, but with light-weight functional tools
- Interpreted
- First-class functions
- Concise --- to a degree alike Python or Ruby, but with the addition of type declarations
- Easily usable by musicians and programmers alike with minimal learning curve
- Uses MIDI as a representation of music
- Single-threaded

Use Case
--------

The algorithm for producing a number in the Fibonacci sequence given its index presents an interesting use case for Apollo. The algorithm is both simple and familiar; however, it yields interesting consequences when return values are translated to musical notes.

*Note that the code below is intended as a sketch of functionality. The syntax used is alike a statically-typed variant of Python for the sake of familiarity, but may not be the exact syntax used by Apollo.*

    def fib(n: Int) -> Int:
        if n == 0:
            play(0)
        elif n == 1:
            play(1)
        else:
            play(fib(n-1) + fib(n-2))

First, a few of Apollo's constructs must be explained in brief. The `play` function maps a given integer value (for example) to a scaled note. The note is then added in sequence to the global representation of a composition and then the value of the integer argument is returned by the function. The composition's representation is alike an array where elements occur in sequential units of time. As the program executes, the composition is translated to MIDI and printed to `stdout` so that it may be redirected into a file.

The result of executing the above code is distinct from simply mapping the play function over a pre-determined section of the Fibonacci sequence. The latter would produce a result to the effect of mapping `play` over the sequence `[0, 1, 1, 2, 3]` if `n = 4`. The former also plays intermediary values in sequence, effectively building a recursive tree of the function's return values in sound. Visually, this would be represented as follows:

*In the tree below, non-terminal nodes represent indices of the Fibonacci sequence for which recusive calls to `fib` are made. Terminal nodes represent return values, which are then propogated up through the tree.*

                                    (4)
                                  /     \
                                 /       \
                                /         \
                              (3)    +     (2)
                             /   \        /   \
                            /     \      /     \
                          (2)  +  (1)  (1)  +  (0)
                         /   \     |    |       |
                       (1) + (0)   1    1       0
                        |     |
                        1     0

The tree above propagates into the following sequence to be played: `[1, 0, 1, 1, 2, 1, 0, 1, 3]`.

By a simple modification of a Fibonacci algorithm (the substitution of `play` for `return`), the programmer is able to produce a sonic representation of an algorithm's *process*, as opposed to the data it produces. Without immediately realizing the tree-structure created in the act of finding a Fibonacci sequence number, the programmer is able to hear the shape of it using Apollo.

We intend to create more complex tooling for Apollo than is demonstrated by this use case. A simple but powerful set of built-in functional tools and more complex (yet abstract) sonic manipulations will enable a rich set of musical experiences to be producible using Apollo. But as with the Fibonacci example, we hope that the process of exploring algorithms through music --- and vice-versa --- will be sonically interesting, revealing of deeper truths regarding the nature of algorithms, and overall, fun.

