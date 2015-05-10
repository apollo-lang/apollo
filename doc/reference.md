% Apollo Language Reference Manual
% **Team 8**
  **System Architect:** Benjamin Matthew Kogan (bmk2130)
  **Language & Tools Guru:** Javier Llaca (jl3960)
  **Project Manager:** Reza Nayebi (rn2324)
  **Tester & Validator:** Roberto Jose De Amorim (rja2139)
  **System Integrator:** Souren Sarkis Papazian (ssp2155)
% March 25, 2015

![](./img/lrm-logo.png)

Apollo Language Reference Manual
================================

Introduction
------------

Apollo is a functional programming language for algorithmic music composition.
It is intended to be usable by a programmer with knowledge of basic functional
constructs and no prior experience with music creation. The fine-details of
synthesizing music are abstracted such that familiar programming types like
integers can be interpreted by the compiler as musical sequences. At the same
time, more experienced musicians can directly manipulate note and chord types
while leveraging Apollo's programming constructs to create novel compositions.
In effect, Apollo empowers the programmer to hear the sound of algorithms and
the musician to compose in code.  An Apollo source program produces as output a
MIDI file, which is a standardized way to store a musical piece.

### Origin of the Name and Logo

Apollo combines the power of source code, or Apollonian logic, with the art of
music, the domain of the god Apollo. The logo is derived from a combination of
the f in functional and the f-hole opening in the body of a cello.

Paradigm
--------

Apollo is a purely functional programming language, and thus incorporates the
following patterns.

### First-class functions

Functions are first-class citizens in Apollo. This means that functions, among
other things, can be passed as function parameters.

### Immutable data

Apollo does not have variables. In other words, once an identifier is bound to
a type and a value, this cannot be changed. As a result, Apollo programs are
easy to reason about and their outputs are deterministic.

Lexical Elements
----------------

Apollo has five types of tokens: keywords, identifiers, constants, operators
and separators.

All language constructs are written in camel-case; user-defined names should
also follow this naming convention.

### Keywords

Keywords are reserved for the language and cannot be used as identifiers.

The list of keywords is:

 * `case`
 * `otherwise`
 * `where`
 * `Int`
 * `Bool`
 * `Pitch`
 * `Duration`
 * `Atom`
 * `Music`

### Identifiers

Identifiers are for naming variables and functions. An identifier is any
lowercase letter followed by any sequence of letters and digits that is not a
keyword.

### Constants

There are two types of constants: integer constants and boolean constants.

#### Integer Constant

Any signed sequence of digits that is in the range of `Int` (see the section on
Data Types).

#### Boolean Constant

Either `True` or `False`.

### Operators

Symbols that indicate operations, for example addition, multiplication, etc.

See Section 4 for more information on operators.

### Separators

Symbols that separate tokens: 

 * `=`
 * `->`
 * `:`
 * `,`
 * `(`
 * `)`
 * `[`
 * `]`
 * `{`
 * `}`
 * `_`

Whitespace is ignored --- and hence is not considered a token --- but serves as
a token separator.

### Comments

 * `--` introuces a comment that terminates with the newline character.
 * `{-` introduces a mutli-line comment.
 * `-}` terminates a multi-line comment.

### Character Set

Apollo officially supports the ASCII character set. Because Haskell supports
UTF-8, non-ASCII characters may be usable in Apollo programs; however, their
behavior is undefined.

Data Types
----------

All data types are named beginning with a capital letter, as to distinguish
them from identifiers.

### Primitive Data Types

 * `Int`: 29 bit signed integer (corresponding to Haskell's `Int` type).
 * `Bool`: boolean value; either `True` or `False`.
 * `Pitch`: integer ranging from 0 to 127, interpreted as a musical
   pitch, or the height of a note on a musical staff. `Pitch` values are
   constructed $mod 128$.
 * `Duration`: non-negative integer indicating a multiple of the smallest
   possible duration (a 64th note, or a 64th of a beat in 4/4 time). Negative
   `Duration` values are constructed as `0`.

Note that, although `Pitch` and `Duration` represent integers, they are not
equivalent to `Int`. In other words, they cannot be used interchangeably.
Type coercion is used in some instances to make the types uniform.

#### Short-hand notation for `Pitch` and `Duration`

When not constructed with integers, `Pitch`es and `Duration`s can be
conveniently expressed in a short-hand notation inspired by musical
conventions.

##### Pitch

A single upper-case letter from A to G indicating the pitch, followed by an
optional `#` or `b` character indicating the accidental (sharp or flat,
respectively), followed by a number from 0 to 9 indicating the octave.

For example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C4          -- C on the fourth octave.  Translates to 60
A#5         -- A#5 on the fifth octave. Translates to 82
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

###### Pitch to Integer Table

The following table represents the mappings of pitches to integers, according
to the MIDI standard.

|oct.| C   | C#/Db | D   | D#/Eb | E   | F   | F#/Gb | G   | G#/Ab | A   | A#/Bb | B   |
|----|-----|-------|-----|-------|-----|-----|-------|-----|-------|-----|-------|-----|
|*0* | 12  | 13    | 14  | 15    | 16  | 17  | 18    | 19  | 20    | 21  | 22    | 23  |
|*1* | 24  | 25    | 26  | 27    | 28  | 29  | 30    | 31  | 32    | 33  | 34    | 35  |
|*2* | 36  | 37    | 38  | 39    | 40  | 41  | 42    | 43  | 44    | 45  | 46    | 47  |
|*3* | 48  | 49    | 50  | 51    | 52  | 53  | 54    | 55  | 56    | 57  | 58    | 59  |
|*4* | 60  | 61    | 62  | 63    | 64  | 65  | 66    | 67  | 68    | 69  | 70    | 71  |
|*5* | 72  | 73    | 74  | 75    | 76  | 77  | 78    | 79  | 80    | 81  | 82    | 83  |
|*6* | 84  | 85    | 86  | 87    | 88  | 89  | 90    | 91  | 92    | 93  | 94    | 95  |
|*7* | 96  | 97    | 98  | 99    | 100 | 101 | 102   | 103 | 104   | 105 | 106   | 107 |
|*8* | 108 | 109   | 110 | 111   | 112 | 113 | 114   | 115 | 116   | 117 | 118   | 119 |
|*9* | 120 | 121   | 122 | 123   | 124 | 125 | 126   | 127 |       |     |       |     |

This notation is described by the regular expression

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
[A-G](#|b)?[0-9]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##### Duration

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
d: Duration = 32    -- 32 64th notes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`Duration` types can be initialized using a short-hand notation, which consists
of three components:

 * A backslash `\`
 * An integer from 1 to 64, denoting the denominator of duration fraction
 * An optional dot `.`, denoting dotted-rhythms

These are some examples:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
\1		-- Whole note.          Translates to 64
\2		-- Half note.           Translates to 32
\4		-- Quarter note.        Translates to 16
\8      -- Eigth note.          Translates to 8
\16     -- Sixteenth note.      Translates to 4
\32     -- Thirtysecond note.   Translates to 2
\64     -- Sixtyfourth note.    Translates to 1
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Using this notation, one can declare a Duration in the following way:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
d: Duration = \4
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This notation can be described by the regular expression

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
\\[0-9]+\.?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that for `Duration` and `Pitch`, initialization to a number outside of the
allowed range will result in a compile-time error.

### Derived Types

Derived types are declared using a constructor function of the same name. The
one exception is lists, which are declared using brackets: `[...]`.

 * `List`: an ordered collection of elements of the same type. The type of the
   list's elements must be declared between brackets in the type-annotation.
   For example, a list of `Int`s:

	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	a: [Int] = [1, 2, 3]
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 * `Atom`: a pair representing a musical sound unit. `Atom` values can be
   constructed in three ways:
    1. `(Pitch, Duration)` represents a note
    2. `([Pitch], Duration)` represents a chord
    3. `(Nil, Duration)` represents a rest

    These are some examples:

	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	note: Atom = (A5, \8)               -- An A5 note (eighth note duration)
	chord: Atom = ([A5, C#6, E6], \2)   -- An A major chord (half note duration)
	rest: Atom = (_, \4)                -- A Rest (quarter note duration)
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 * `Music`: a list of lists of `Atom`s. Each list of `Atom`s in a `Music` value
   represents a musical part -- a melodic line or a chordal accompaniment, for
   instance.

	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	aMajor: Atom = ([A5, C#5, E5], \4)
	bMinor: Atom = ([B5, D#5, F#5], \4)
	eMajor: Atom = ([E5, G#5, B5], \4)

	back: [Atom] = [aMajor, bMinor, eMajor]
	lead: [Atom] = [(A5, \4), (F#5, \4), (E4, \4)]

	song: Music = [lead, back]
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Operators
---------

### Arithmetic Operators

Arithmetic operators are only defined for numeric primitive data types (i.e.,
`Int`, `Pitch`, and `Duration`).

#### `a + b`

The sum of `a` and `b`.

| Int       | +         | Int       | Int       |
| Int       | +         | Pitch     | Pitch     |
| Pitch     | +         | Int       | Pitch     |
| Duration  | +         | Duration  | Duration  |

#### `a - b`

The sum of `a` and `b`.

| Int       | -         | Int       | Int       |
| Pitch     | -         | Int       | Pitch     |
| Duration  | -         | Duration  | Duration  |

#### `a * b`

| Int       | *         | Int       | Int       |
| Int       | *         | Duration  | Duration  |
| Duration  | *         | Int       | Duration  |

#### `a / b`

| Int       | *         | Int       | Int       |
| Int       | *         | Duration  | Duration  |
| Duration  | *         | Int       | Duration  |

#### `a % b`
#### `- a`

The behavior of each operator is defined based on the type(s) to which it is
applied. The following table outlines the possible combinations of arithmetic
operations:

| Type of a | Operator  | Operand B | Result    |
|-----------|-----------|-----------|-----------|
| Int       | +         | Int       | Int       |
| Int       | +         | Pitch     | Pitch     |
| Pitch     | +         | Int       | Pitch     |
| Duration  | +         | Duration  | Duration  |
| Int       | -         | Int       | Int       |
| Pitch     | -         | Int       | Pitch     |
| Duration  | -         | Duration  | Duration  |
| Int       | *         | Int       | Int       |
| Int       | *         | Duration  | Duration  |
| Duration  | *         | Int       | Duration  |
| Int       | /         | Int       | Int       |
| Duration  | /         | Int       | Duration  |
| Int       | %         | Int       | Int       |

When an operator is applied to a type with which it is not compatible, a
compile-time error is triggered.

### Comparison Operators

#### `a == b`
#### `a != b`
#### `a < b`
#### `a > b`
#### `a <= b`
#### `a >= b`

### Boolean Operators

#### `!a`
#### `a && b`
#### `a || b`

### List operators

#### `!list`
#### `h@list`
#### `t@list`
#### `a :: list`

### Operator Precedence and Associativity

| Operator | Description           | Precedence | Associativity | Type          |
|----------|-----------------------|------------|---------------|---------------|
| `-`      | unary minus           | 7          | right         | Unary         |
| `!`      | negation              | 7          | right         | Unary         |
| `h@a`    | list head             | 7          | right         | Unary         |
| `t@a`    | list tail             | 7          | right         | Unary         |
| `*`      | multiplication        | 6          | left          | Binary        |
| `/`      | division              | 6          | left          | Binary        |
| `%`      | modulo                | 6          | left          | Binary        |
| `+`      | addition              | 5          | left          | Binary        |
| `-`      | subtraction           | 5          | left          | Binary        |
| `::`     | cons                  | 4          | right         | Binary        |
| `==`     | equality              | 3          | left          | Binary        |
| `!=`     | not equality          | 3          | left          | Binary        |
| `<`      | less than             | 3          | left          | Binary        |
| `>`      | greater than          | 3          | left          | Binary        |
| `<=`     | less than or equal    | 3          | left          | Binary        |
| `>=`     | greater than or equal | 3          | left          | Binary        |
| `&&`     | logical AND           | 2          | left          | Binary        |
| `||`     | logical OR            | 1          | left          | Binary        |
| `=`      | definition            | 0          | N/A[^assn]    | Binary        |

[^assn]: nested-definition is not allowed, so associativity rules are not
applicable to the definition operator.

Definitions
-----------

Definitions bind an identifier to a type and a value. The syntax for
definitions is the following:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
<id>: <type> = <expression>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If, for example, we want to define an `Int` *x* whose value is 4, we would
write the following line:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
x: Int = 4
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Definitions have no value, that is, they do not return anything. Therefore,
nested definitions are invalid:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
y: Int = (x: Int = 4)     -- invalid
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Because names are immutable in Apollo, any name must be defined in the same
line that it is declared. Declaring a name without a value is not allowed, and
so the following is invalid:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
y: Int                  -- invalid
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Functions
---------

Functions are first-class in Apollo and are treated like values of any other
type.

They are defined as follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
add: (x: Int, y: Int) -> Int = x + y
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This defines a function `add` which takes an `Int` `x` and an `Int` `y` and
returns the sum of `x` and `y`.

### Types

Functions take one or more types and return another type. Like functions in
mathematics, they map elements in one or more sets to an element in another
set. Consider the following function that takes an integer x and returns its
square.
    
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
square: (x: Int) -> Int = x * x
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We say the type of `square` is `(Int) -> Int`. It maps from the set of integers
to the set of integers.

### Recursion

Functions can be recursive, that is, they can call themselves. Consider the
following function for computing the factorial of an integer `n`:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
factorial: (n: Int) -> Int = case (n == 0) 1 otherwise n * factorial(n - 1)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is a declarative implementation of factorial. In other words, instead of
specifying how to compute, it specifies what should be computed. The functions
reads as "The factorial of 0 is 1, otherwise it is n times the factorial of n -
1."
 
Recursive functions can be used to simulate looping. Consider the following
function, which adds an `Int` `x` to an `Int` `n`:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
addX: (n: Int, x: Int) = case (x == 0) n
                         otherwise     addX(a + 1, x - 1)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Higher-Order Functions

Functions in Apollo can be passed as parameters to other functions. Example:
        
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
twice: (f: (Int) -> Int, x: Int) -> Int = f(f(x))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The twice function takes a function `f`, which takes an `Int` and returns an
`Int`, and applies it twice to the argument x. Note how the parameters to `f`
in `twice` are not named, only the types are included. In general, When
defining a function `f` that takes a function `g` as a parameter, the
parameters to `g` need not be named.

Now we can use our two functions to declare a new function, pow4, which takes
an integer x and returns x^4.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pow4: (x: Int) -> Int = twice(square, x)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Lambda Expressions

TODO

Expressions
-----------

Expressions are statements with a value. Because Apollo imposes functional
purity, a value expression can always be replaced with its value.

The following are examples of expressions in Apollo:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
3 + 4
x * x
y > 18
[1,2,3]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### Blocks

A block is an expression delimited by curly braces. Using the `where` keyword,
blocks can be used to declare local-scope auxiliary values or functions. In
other words, blocks consist of an expression followed by a list of definitions.

The value of a block is the value of the expression it contains.

Consider the following two versions of a function that computes the surface
area of a cylinder:

##### One - using a block:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cylinderArea: (r: Int, h: Int) -> Int = {
    sideArea + 2 * baseArea
    where
        sideArea: Int = 2 * pi * r * h
        baseArea: Int = pi * r * r
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##### Two - without a block:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cylinderArea: (r: Int, h: Int) -> Int = (2 * pi * r * h) + 2 * (pi * r * r)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Both versions produce the same result, but the first one is arguably more
readable and modular.

Consider the following implementation of factorial that uses a block to
define a tail-recursive auxiliary function:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
factorial: (n: Int) -> Int = {
    aux(n, 1)
    where
        aux: (n: Int, acc: Int) -> Int = {
            case (n == 0)   acc
            otherwise       aux(n - 1, acc * n)
        }
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Type Coercion

Type coercion allows us to conveniently go from `Int` to `Pitch` or `Duration`,
without the need for an explicit conversion function. Coercion works
differently for a naked list expression and for a definition. In the case of
naked expressions, any list containing either one or more `Pitch` elements and
`Int`s is converted to a `[Pitch]` list:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
[1,C4, 5]                   -- Evaluates to [(1),(60),(5)]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The same holds for a list containing `Int` and `Duration` elements:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
[1,\4, 5]                   -- Evaluates to [(1),(60),(5)]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For definitions however, the compiler looks at the type of the list that is
being defined. If it is a `[Pitch]` any `Int` in the list is converted to a
`Pitch`, e.g.:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
a: [Pitch]  = [1,2,3]       -- Defines a as [(1),(2),(3)] ([Pitch])
a: [Duration] = [1,2,3]     -- Defines a as [(1),(2),(3)] ([Duration])
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

However, if we remove the additional context (i.e., the expected type), the
list interpreted as `[Int]`:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
[1,2,3]                     -- Evaluates to [1,2,3]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Control Flow
------------

A conditional expression is a series of one or more `case` statements, followed
by an `otherwise` statement. A case statement must be followed by a single
parenthesis-enclosed expression that must evaluate to a `Bool`. The first
`case` statement whose condition evaluates to `True` will be evaluated and the
value it returns will be the value for the entire conditional statement. If no
preceding case condition evaluates to `True`, the `otherwise` expression will
be evaluated. Note that this means if multiple case conditions evaluate to
`True`, only the first of these case expressions will be evaluated.

Consider the following example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
foo: Int = case (1 > 2) 1
           case (False) 2
           case (True)  3
           case (True)  4
           otherwise    5
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Here foo is an integer whose value is determined by a case statement. The first
two case statements evaluate to `False` and their return value is ignored. The
third statement is the first one to evaluate to `True` and foo is therefore
bound to the value `3`.

Program Structure
-----------------

A program in Apollo is consists of one or more valid statements. A statement in
Apollo is either an expression or a definition.

The lifetime of an Apollo program begins in the main value, which is of type
Music and is required for a program to compile. For example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
main: Music = [[(`A5, \4)]]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This program will compile to a MIDI file containing a single note -- an A in the
fifth octave with a quarter-note duration.

Scoping
-------

### Blocks

An execution block is a list of statements enclosed between the starting curly
brace `{` and the respective closing brace `}`. Blocks can be nested. Names
defined within a block have a scope limited to that block. Names defined in a
scope within which a given block is contained are accessible within that block
as long as that block does not redefine the same name. If the given block
redefines a name defined in a containing block, the former shadows the latter
(i.e. the former is unaffected by the later, but it is not accessible within
the block in which the later is accessible).

### Functions

Since a function is effectively a parameterized name assigned to a block, the
body of a function follows the rules of block scoping. This means that a
function defined within a function creates a new block inside the parent
function's block.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
f: (x: Int) -> Int = {
    g(x) + 3                        -- x refers to parameter of f
    where
        g: (x: Int) -> Int = x * x  -- x refers to parameter of g
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Lambda Expressions

Following the conventions of lambda expressions

Standard Library
----------------

The Apollo standard library, or prelude, contains functions that can be called
by the user in any program. These include canonical list functions like `map`,
`reduce`, `fold`, and `zip` that can be used to leverage the power of
functional programming for music composition.

Every time an Apollo program is run, the prelude is loaded into the runtime
environment.

