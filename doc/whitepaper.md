Apollo Language Whitepaper
==========================

Contributors:

- Roberto Jose De Amorim (rja2139)
- Benjamin Matthew Kogan (bmk2130)
- Javier Llaca (jl3960)
- Reza Nayebi (rn2324)
- Souren Sarkis Papazian (ssp2155)

Introduction
------------

Apollo is a programming language for algorithmic and musical composition. The language provides an interface to leverage light-weight functional paradigms in order to produce a target program that generates a musical output when run. Apollo is intended to be usable by a programmer with knowledge of basic functional constructs and no prior experience with music creation. The fine-details of synthesizing music are abstracted into a familiar typed-interface. Instead of direct note-manipulation, common programming types like integers and characters are mapped to musical values when interpreted. In effect, Apollo thus allows the programmer to *hear the sound of an algorithm*.

Rationale
---------

There are a number of extant music programming languages such as [RTcmix][] and [SuperCollider][]. Apollo shares with these languages an interest in providing an abstraction for music creation to the programmer. Apollo differs from these in the depth and the intent of its abstractions. Virtually all common music programming languages provide a direct interface to sonic manipulation on the level of notes and frequencies. This allows the programmer to *act* as a musician by writing code.

Apollo, in contrast, abstracts sonic details in favor of simplicity. The specifics of musical articulation are eschewed. In the place of deep --- but complex --- sonic articulation, the programmer should be able to interpret familiar data-values using a `play` function or optionally act on a discrete note-type. Instead of conceiving of specific musical compositions prior to programming, the user conceives of a program that is transformed into music during execution. Apollo thus takes a more experimental approach where the programmer is able to *become* a musician through the act of programming.

[RTcmix]: http://rtcmix.org
[SuperCollider]: http://supercollider.sourceforge.net

Language Features
-----------------

- Statically-typed
- Imperative, but with light-weight functional tools
- Interpreted
- First-class functions
- Concise --- to a degree alike Python or Ruby, but with the addition of explicit typing
- Single-threaded
- Easily usable by musicians and programmers alike with minimal learning curve
- Abstracted
- Simple

Implementation
--------------

- Maintains a record of notes in global state that are ultimately used to create a music file
- Uses MIDI as a representation of music
- reasoning about music logically by leveraging light-weight functional programming
- code as music
- ability to reach to note values, or to interpret

Use Cases
---------



