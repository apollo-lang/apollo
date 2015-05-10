Apollo Language Whitepaper
==========================

Team number 8. Contributors:

- **Tester & Validator:** Roberto Jose De Amorim (rja2139)
- **System Architect:** Benjamin Matthew Kogan (bmk2130)
- **Language & Tools Guru:** Javier Llaca (jl3960)
- **Project Manager:** Reza Nayebi (rn2324)
- **System Integrator:** Souren Sarkis Papazian (ssp2155)

Implementation language: Haskell.

Introduction
------------

Apollo is a functional programming language for algorithmic and musical composition. The language provides an interface to leverage functional paradigms in order to produce a target program that generates a musical output when run. Apollo is intended to be usable by a programmer with knowledge of basic functional constructs and no prior experience with music creation. The fine-details of synthesizing music are abstracted into a familiar typed-interface. Instead of direct note-manipulation, common programming types like integers are mapped to musical values when interpreted.

Rationale
---------

There are a number of extant music programming languages such as [RTcmix][] and [SuperCollider][]. Apollo shares with these languages an interest in providing an abstraction for music creation to the programmer. Apollo differs from these in the depth and the intent of its abstractions. Virtually all common music programming languages provide a direct interface to sonic manipulation on the level of notes and frequencies. This allows the programmer to *act* as a musician by writing code.

Apollo, in contrast, abstracts sonic details in favor of simplicity. The specifics of musical articulation are eschewed. In the place of deep --- but complex --- sonic articulation, the programmer should be able to interact with familiar data-values and combine them by assigning them to a `main` variable that will be translated into music. Apollo thus takes a more experimental approach where the programmer is able to *become* a musician through the act of programming. 

[RTcmix]: http://rtcmix.org
[SuperCollider]: http://supercollider.sourceforge.net

Language Features
-----------------

- Simple
- Abstract
- Statically-typed
- Functional
- Interpreted
- Easily usable by musicians and programmers alike with minimal learning curve
- Uses MIDI as a representation of music
- Single-threaded

Use Case
--------

The algorithm for producing a number in the Fibonacci sequence given its index presents an interesting use case for Apollo. The algorithm is both simple and familiar; however, it yields interesting consequences when return values are translated to musical notes.


```
#tempo 160

fibonacci: (n: Int) -> Int = 
    case (n <= 1) 1 
    otherwise     fibonacci(n - 1) + fibonacci(n - 2)

fibSeq: (n: Int) -> [Int] = mapII(fibonacci, sequence(0, n))

mySeq: [Pitch] = mapII(\x: Int -> Int: x + 40, fibSeq(20))

notesA: [Pitch] = replicateP(mySeq, 20)

rhythm: [Duration] = uniform(\8, lengthP(notesA))

partA: [Atom] = zip(notesA, rhythm)

main: Music = [partA]
```

This example is very representative of the kind of programs that can be written in Apollo. First the global tempo is defined as `160` (BPM) using the global `#tempo`, which can only be defined once in a program. What follows is the implementation of the well-known `fib` function (returning the nth Fibonacci number) in Apollo. The function is simple but demonstrates the simplicity and readability of programs in Apollo. Next the function `fibSeq` is used to create a list by mapping the `fibonacci` function over `sequence(0,n)` (`[0,1,...,n-1]`) and stores the result as a list of pitches using the `mapII` function with a lambda expression to transpose these pitches by 20 semi-tones. Next a uniform rhythm of eigth notes is created using the `uniform` function. The list of pitches and our rhythm are zipped to form two lists of atoms (which are notes, chords or rests). Finally, `main` (a reserved word for a name that points to the music that is to be created) is defined as a list containing our notes. The result of executing the above code will be a MIDI file containing our music, namely the (transposed) first 20 numbers in the Fibonacci sequence, repeated 20 times. 

We thus see the power provided by functional programming for music creation in Apollo. The ability to easily define functions to interact with musical types in an intuitive way makes Apollo easy to learn and understand. Despite its simplicity, Apollo can also produce a rich set of musical experiences and can therefore be used both by music-loving programmers and by musicians fascinated by the power of algorithms.

