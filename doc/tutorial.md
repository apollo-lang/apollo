% Apollo Language Tutorial
% **Team 8**
  **Tester & Validator:** Roberto Jose De Amorim (rja2139)
  **System Architect:** Benjamin Matthew Kogan (bmk2130)
  **Language & Tools Guru:** Javier Llaca (jl3960)
  **Project Manager:** Reza Nayebi (rn2324)
  **System Integrator:** Souren Sarkis Papazian (ssp2155)
% March 25, 2015

\pagebreak

![](./img/lrm-logo.png)

Apollo Language Tutorial
========================

Introduction
------------

Apollo is a functional programming language for algorithmic music composition. An Apollo source program produces as output a MIDI file, which is a standardized way to store a musical piece.

This document serves as a short tutorial for the language, showing the essential elements necessary to write and execute Apollo programs.

Hello World (Compile and Run your first program)
------------------------------------------------

Let's look at a very basic program in Apollo, namely one that creates a MIDI file containing a single note.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
main: Music = [[(C5, \4 )]]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Looking at this program a little more carefully, we see that it essentially initializes main, a special variable of type `Music` that contains what will eventually be written to our MIDI file. `Music` is in fact just a list of lists of `Atom`s. An `Atom`, in turn, is simply a note, a chord or a rest (here a single note `( C5, \4 )`), where the first element corresponds to the pitch C5 and the second one to the duration, here 1/4th of a beat. We could just use integers to initialize a note, but the macros presented here provide a more readable and intuitive notation. We will go more in detail about the different types available in Apollo in the following section.

You can put this source code in a file, say hello.ap. Assuming that you are in a UNIX environment, you would enter the following command:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
$ apollo hello.ap
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

to get the file `main.mid`, the MIDI file containing your music. Now you can play this MIDI file, use it to control your MIDI-compatible instruments or anything else that crosses your mind!

Data Types
----------

### Non-Musical Data Types

There are three non-musical data types: `Int` and `Bool`, which are primitives, and `List`, which is derived.

 * `Int`: integer type, ranging from -2^29 to 2^29-1. For example:

	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	a: Int = 3
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 * `Bool`: boolean type, with value of either `True` or `False`. For example:

	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	b: Bool = False
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 * `List`: an ordered colleciton of elements of the same type. For example:

	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	a: [Int] = [1, 2, 3]
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Musical Data Types

Apollo has a series of data types that are used to make music.

The most primitive types are `Pitch`es and `Duration`s.

A `Pitch` describes the frequency of a note. It can constructed using a pitch literal or an integer value.

Initializing a pitch with a pitch literal:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sol: Pitch = G4   -- c in the fourth octave
mib: Pitch = Eb4  -- e flat in the fourth octave
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A pitch literal is: a single capitalized character indicating the note, followed by an optional `#` or `b` character indicating the accidental, followed by a number indicating the octave.

Alternatively, we can define the note using an integer that indicates its offset from the first (0th) note i.e. *c* in the zero octave (`` C0 ``):

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sol: Pitch = 55
mib: Pitch = 51
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Here is a table of integer to pitch mapping:

|    | c   | c#/db | d   | d#/eb | e   | f   | f#/gb | g   | g#/ab | a   | a#/bb | b   |
|----|-----|-------|-----|-------|-----|-----|-------|-----|-------|-----|-------|-----|
| 0  | 0   | 1     | 2   | 3     | 4   | 5   | 6     | 7   | 8     | 9   | 10    | 11  |
| 1  | 12  | 13    | 14  | 15    | 16  | 17  | 18    | 19  | 20    | 21  | 22    | 23  |
| 2  | 24  | 25    | 26  | 27    | 28  | 29  | 30    | 31  | 32    | 33  | 34    | 35  |
| 3  | 36  | 37    | 38  | 39    | 40  | 41  | 42    | 43  | 44    | 45  | 46    | 47  |
| 4  | 48  | 49    | 50  | 51    | 52  | 53  | 54    | 55  | 56    | 57  | 58    | 59  |
| 5  | 60  | 61    | 62  | 63    | 64  | 65  | 66    | 67  | 68    | 69  | 70    | 71  |
| 6  | 72  | 73    | 74  | 75    | 76  | 77  | 78    | 79  | 80    | 81  | 82    | 83  |
| 7  | 84  | 85    | 86  | 87    | 88  | 89  | 90    | 91  | 92    | 93  | 94    | 95  |
| 8  | 96  | 97    | 98  | 99    | 100 | 101 | 102   | 103 | 104   | 105 | 106   | 107 |
| 9  | 108 | 109   | 110 | 111   | 112 | 113 | 114   | 115 | 116   | 117 | 118   | 119 |
| 10 | 120 | 121   | 122 | 123   | 124 | 125 | 126   | 127 |       |     |       |     |

But a pitch can't be heard without a duration, so we need to define what a duration is. We will then combine the pitch and duration to construct a note.

A `Duration` quantifies the length of a note, chord or rest in time. It can be defined using a multiple of the smallest time division (1/64th of a beat) or a fraction of a whole note (defined in the context of a 4/4 time signature by default).

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
short: Duration = \4  -- a quarter note (fraction notation)
long:  Duration = 64  -- a whole note   (multiple notation)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Now we're ready to construct our first note!

An Atom is something that can be played. So let's put our note into an Atom. To do this we make a tuple consisting of a `Pitch` (or `Pitch`es or `_` indicating silence) and a `Duration`:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
n1: Atom = (sol, short)
n2: Atom = (mi, long)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We could've alternatively done:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
n1: Atom = (G4, \4)
n2: Atom = (Eb4, 64)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Or if we wanted to construct our pitch using an integer:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
n1: Atom = (55, \4)
n2: Atom = (51, 64)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Let's make a MIDI out of our note!

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
main: Music = [[n1]]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We will explain what `Music` is soon. For now, let us talk about `Atom`s.

If we want to play both our notes simultaneously, we can construct a chord `Atom`. A chord `Atom` consists of a list of pitches and a single duration.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
c1: Atom = ([n1, n2], long)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can now play this chord:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
main: Music = [[c1]]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You've probably realized by now that an `Atom` is either a note or a chord, but it can also be a rest.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
r1: Atom = (_, long)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Now that we know how to make `Atoms`, we can put several `Atoms` in sequence to make a melody as a `[Atom]`.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
p: [Atom] = [n1, n1, n1, n2]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

And finally, we can use a list of `[Atom]`s to make `Music`!

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sol: Pitch = G4              -- c in the fourth octave
mib: Pitch = Eb4             -- e flat in the fourth octave

short: Duration = \4          -- a quarter note
long: Duration = \1           -- a whole note

n1: Atom = (sol, short)
n2: Atom = (mib, long)

p1: [Atom] = [n1,n1,n1,n2]

main: Music = [p1]     -- Beethoven's Symphony No. 5
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the example above we only have one part, but if we had two parts in the list, they would be played simultaneously.

For example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
do: Pitch = C4
re: Pitch = D4
mi: Pitch = E4
fa: Pitch = F4
sol: Pitch = G4

n1: Atom = (do, \4)
n2: Atom = (re, \4)
n3: Atom = (mi, \4)

c1: Atom = ([do, mi, sol], 64)

p1: [Atom] = [n1, n2, n3]
p2: [Atom] = [c1]

main: Music = [p1, p2]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Conditionals
------------

Conditionals are expressed using case statements.

For example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
i: Int = 0
j: Int = case(i == 0)
             1
         case(i == 1)
             i * 2
         otherwise
             2
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Here we illustrate another important principle in Apollo. A block has a value that is equal to the value of its last line. Therefore, this case statement returns `1` if `i` is `0`, `i*2` if `i` is `1` and `2` otherwise.

Functions
---------


Functions and values are declared using the same syntax. Functions, however, have different types --- they take one or more types and return another type. Like mathematical functions, they map elements in one or more sets to an element in another set. Let's define a function that takes an integer x and returns its square.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
square: (x: Int) -> Int = x * x
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A function consists of a declaration and definition. The declaration of square is

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
square: (x: Int) -> Int
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

which reads as "square is a function that takes an `Int` whose identifier is x and returns an `Int`".

The definition is whatever is to the right side of the assignment operator:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
x * x
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that declarations cannot exist on their own; they require a definition.

Functions in Apollo can be passed as parameters to other functions. Let's write a function that applies a function twice to an integer.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
twice: (f: (Int) -> Int, x: Int) -> Int = f(f(x))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The twice function takes a function f and applies it twice to the argument `x`. Now we can use our two functions to declare a new function, `pow4`, which takes an integer `x` and returns `x^4`.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pow4: (x: Int) -> Int = twice(square, x)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Functions can be recursive, that is, they can call themselves. Consider the following function for computing the factorial of an `Int` *n*:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
factorial: (n: Int) -> Int =
    case (n == 0) 1 otherwise n * factorial(n - 1)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As in all recursive functions, we need a base case to prevent infinite recursive calls. For this we use conditional statements. The function is reads as "the factorial of `n` is `1` if `n` is `0`, otherwise it is `n` times the factorial of `n - 1`."

Lists
-----

Lists are the essential data structure in Apollo. They provide an easy way to manipulate pitches, notes, chords and rhythms, making composition in Apollo very straightforward for the programmer. Apollo provides useful functions such as map, reduce, and zip that make manipulating lists easy.

For example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
mult2: (x: Int) -> Int = x * 2
r: [Duration] = mapII(mult2, [16,32,32])
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In this example, the `mult2` function takes an integer and multiplies it by two. The mapII function (for map from `Int` to `Int`) then takes the `mult2` function and a list of integers and multiplies every integer by 2, effectively slowing down the rhythm by a half.

Compiling and Running
---------------------

To compile a valid Apollo source code file (for example, main.ap), simply run the Apollo compiler with the path to the source file as its first command line argument:

~~~
$ apollo main.ap
~~~

*Note that the above assumed that the `apollo` executable is in the user’s `$PATH` environment variable*

This will begin the Apollo interpreter, thereby converting Apollo source code into MIDI. If the program is free of runtime errors, terminates in a finite amount of time, and assigns `Music` to the variable `main` then a MIDI will be created. By default, this file will be named main.mid. The output file can then be played through any MIDI player.

To input source code into the Apollo compiler via stdin, use `-` as the only argument:

~~~
$ echo "main: Music = [[ (C5, \4 ) ]]" | apollo -
~~~

To specify a different name or location for the output MIDI file, use the `-o` flag:

~~~
$ apollo main.ap -o symphony.mid
~~~

To get more information on Apollo’s options, use the `--help` flag:

~~~
$ apollo --help
~~~
