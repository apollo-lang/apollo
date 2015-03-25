% Apollo Language Reference Manual
% **Team 8**
  **Tester & Validator:** Roberto Jose De Amorim (rja2139)
  **Language & Tools Guru:** Benjamin Matthew Kogan (bmk2130)
  **System Integrator:** Javier Llaca (jl3960)
  **Project Manager:** Reza Nayebi (rn2324)
  **System Architect:** Souren Sarkis Papazian (ssp2155)
% March 25, 2015

![](./img/lrm-logo.png)

Apollo Language Reference Manual
================================

Introduction
------------

Apollo is a functional programming language for algorithmic musical
composition. Apollo is intended to be usable by a programmer with knowledge of
basic functional constructs and no prior experience with music creation. The
fine-details of synthesizing music are abstracted such that familiar
programming types like integers can be interpreted by the compiler as musical
sequences. At the same time, more experienced musicians can directly manipulate
note and chord types while leveraging Apollo's programming constructs to create
novel compositions. In effect, Apollo empowers the programmer to hear the sound
of algorithms and the musician to compose in code.

**TODO:** explanation of flow of a program; a composition (notes /
abstractions) -> midi at end

### Why is it called Apollo?

Apollo combines the power of source code, or Apollonian logic, with the art of
music, the domain of the god Apollo. The logo is derived from a combination of
the f in functional and the f-hole opening in the body of a cello.

Functional Constructs
---------------------

Since the functional programming paradigm encompasses a number of different
programming constructs, it is useful to describe the manner in which Apollo is
a functional language.

**TODO**: functional purity in scope making a typical program, immutability; IO
handled by frame

Lexical Elements
----------------

Apollo has seven types of tokens: keywords, identifiers, constants, literals,
operators and separators.

Whitespace and comments are ignored.

All language constructs are written in camel-case; user-defined names should
also follow this naming convention.

### Keywords

Keywords are reserved for the language and cannot be used as identifiers.

The list of keywords is:

 * `Int`
 * `Bool`
 * `True`
 * `False`
 * `case`
 * `otherwise`
 * `List`
 * `Pitch`
 * `Note`
 * `Chord`
 * `Rest`
 * `Atom`

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

### Literals

There are two types of literals: pitch literals and duration literals.

#### Pitch Literal

A backtick, followed by a single letter character indicating the note, followed
by an optional `#` or `b` character indicating the accidental (sharp or flat,
respectively), followed by a number indicating the octave.

For example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
`a#5
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The backtick indicates that you are defining a pitch.

The character for the note must be in the range `[a-g]`. In this case it is the
note `a`.

Not including an accidental indicates that the note is natural. In this case we
make the pitch a sharp by using `#`. The integer for the octave must be in the
range `[0-10]`. In this case we use the pitch `a` in the fifth octave.

This notation is inspired by the way notes are defined in MIDI.

#### Duration Literal

A backslash, followed by a natural number indicating the fraction of a whole
note.

For example, a quarter note:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
\4
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Operators

Symbols that indicate operations, for example addition, multiplication, etc.

See Section 4 for more information on operators.

### Separators

Symbols that separate tokens. They are: `(`, `)`, `[`, `]`, `{`, `}`, and `,`.

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
 * `Pitch`: a type alias for an `Int` that ranges from 0 to 127, interpreted
    as a musical pitch. A `Pitch` may be initialized using an integer or a
    macro that indicates note, accidental, and ocatave:

	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	d: Pitch = 123  -- integer initialization
	d: Pitch = `d#3 -- macro initialization
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 * `Duration`: describes the duration of a `Note` or `Chord` and is also a type
   alias for `Int`, ranging from 1 to 256, indicating the multiple of the
   smallest possible duration (which itself is a 64th note or a 64th of a beat
   in 4/4 time). For example, a duration of 64 would be one beat. A `Duration`
   can also be declared as a literal using a backward slash and an integer. The
   integer denotes the fraction of a whole note. Duration literals are
   syntactic sugar and are thus an optional alternative to using only integers
   to declare duration. See the section on macros for more information.

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

 * `Atom`: a polymorphic type indicating a musical sound-unit. An `Atom` can be
   initialized to either a `Note`, a `Chord`, or a `Rest`. For example:

	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	n: Atom = Note(`a5, \8)
	c: Atom = Chord([`a5, `c#5, `e5], \4)
	r: Atom = Rest(\4)
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 * `Note`: a tuple consisting of a `Pitch` and a `Duration`. For example:

	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	n: Atom = Note(`a5, \4)
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 * `Chord`: a tuple consisting of a list of `Pitch`es and a `Duration`. For
   example:

	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	c: Atom = Chord([`a5, `c#5, `e5], \4)
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 * `Rest`: a `Rest` is simply a `Duration` indicating a space in which no notes
   are played. For example:

	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	r: Atom = Rest(\4)
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 * `Rhythm`: a list of `Duration`s. For example:

	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	r: Rhythm = Rhythm([\4, \8, \8])
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 * `Part`: a `Part` is a list of `Atoms`. This is useful for distinguishing
   separate but simultaneously-occurring lines of musical thought. For example:

	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	p: Part = Part([Note(`a5, 1), Note(`c#5, 2)])
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 * `Music`: the `Music` data type is initialized with a list containing all
   `Part`s in the composition. When the output MIDI file is generated, all
   parts contained in a `Music` element are sounded simultaneously. For
   example:

	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	aMajor: Atom = Chord([`a5, `c#5, `e5])
	bMinor: Atom = Chord([`b5, `d#5, `f#5])
	eMajor: Atom = Chord([`e5, `g#5, `b5])

	back: Part = Part([aMajor, bMinor, eMajor])
	lead: Part = Part([Note(`a5, 10), Note(`b5, 10), Note(`c4, 10)])

	song: Music = Music([lead, back])
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Operators
---------

The behavior of each operator is defined based on the type(s) to which it is
applied. Certain operators such a `/` (integer division) are defined only for a
single pair of types (in this case, two `Ints`). Other operators are overloaded
such that their behavior depends on the types to which it is applied. For
example, the `+` operator preforms infix addition when applied to a pair of
`Int`s, whereas it preforms list concatenation when applied to a pair of
`List`s.

When an operator is applied to a type with which it is not compatible, a
compile-time error is triggered.

### Arithmetic Operators

#### `expression * expression`

If the two expressions are of type `Int`, `*` corresponds to regular
multiplication. If the first expression is a `List` and the second expression
is an `Int`, `*` concatenates that list to itself as many times as specified by
the second expression.

#### `expression / expression`

The two expressions must be of type `Int`. The operator yields the integer
quotient of the first expression divided by the second expression.

#### `expression % expression`

The two expressions must be of type `Int`. The operator yields the integer
remainder of the first expression divided by the second expression.

#### `expression + expression`

When the two expressions are of type `Int`, the result is addition. When the
two expressions are `List`s, `+` corresponds to list concatenation and
concatenates the second list to the end of the first.

#### `expression - expression`

The two expressions must be of type `Int`. The operator yields the subtraction
of the second expression from the first expression.

### Boolean Operators

#### `expression == expression`

Returns `True` if and only if the expressions have the same value, which can be
of any integral type (`Int`, `Pitch`, `Duration`) or a `List`. In the latter
case, this is evaluated in terms of element-by-element value of a `List` rather
than a memory-reference value, for example.

#### `expression != expression`

Returns `True` if and only if the expressions have different values, which
could be of any integral type (`Int`, `Pitch`, `Duration`) or a `List`. In the
latter case, this is evaluated in terms of element-by-element value of a `List`
rather than a memory-reference value, for example.

#### `expression > expression`

If the expressions both have integral type, then `>` yields `True` if and only
if the expression on the left side is larger than the expression on the right
side

#### `expression < expression`

If the expressions both have integral type, then `<` yields `True` if and only
if the expression on the left side is smaller than the expression on the right
side

#### `expression >= expression`

If the expressions both have integral type, then `>=` yields `True` if and only
if the expression on the left side is larger than or equal to the expression on
the right side

#### `expression <= expression`

If both expressions have integral type, then `<=` yields `True` if and only if
the expression on the left side is smaller than or equal to the expression on
the right side

#### `expression && expression`

If both expressions are of `Bool` type, then `&&` yields `True` if and only if
both expressions evaluate to `True`; otherwise, the entire expression evaluates
to `False`.

#### `expression || expression`

If both expressions are of `Bool` type, then `||` yields true if at least one
of the expressions evaluates to `True`. It yields `False` if and only if both
expressions are `False`.

### Operator Precedence and Associativity

| Operator | Description           | Precedence | Associativity |
| -------- | --------------------- | ---------- | ------------- |
| `-`      | unary minus           | 7          | N/A           |
| `!`      | not                   | 7          | N/A           |
| `*`      | multiplication        | 6          | left          |
| `/`      | division              | 6          | left          |
| `%`      | modulo                | 6          | left          |
| `+`      | infix addition        | 5          | left          |
| `-`      | infix subtraction     | 5          | left          |
| `+`      | list concatenation    | 4          | right         |
| `==`     | equality              | 3          | left          |
| `!=`     | not equality          | 3          | left          |
| `<`      | less than             | 3          | left          |
| `>`      | greater than          | 3          | left          |
| `<=`     | less than or equal    | 3          | left          |
| `>=`     | greater than or equal | 3          | left          |
| `&&`     | logical AND           | 2          | left          |
| `||`     | logical OR            | 1          | left          |
| `=`      | assignment            | 0          | N/A[^assn]    |

[^assn]: multiple-assignment is not allowed, so associativity rules are not
applicable to the assignment operator

Functions
---------

Functions are first-class in Apollo and are treated like values of any other
type.

They are defined as follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
add: (x: Int, y: Int) -> Int = x + y
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Functions can be recursive, that is, they can call themselves. They can also be
nested, that is, a function can contain one or more functions within itself.

Looping
-------

Looping is enabled using recursion. To create a subroutine that should loop, a
recursive function can be defined. A simple example of a recursive loop:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
addX: (foo: Int, x: Int) = case x == 0 { foo }
                           otherwise   { addX(foo + 1, x - 1) }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Expressions
-----------

Apollo expressions can be broadly classified into two categories: assignments
and values.

### Assignment Expressions

An assignment consists of an identifier, a type, and a value.

The syntax for assignments is the following:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
<id>: <type> = <value>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If, for example, we want to declare an `Int` *x* whose value is 4, we would
write the following line:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
x: Int = 4
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that assignment expressions have no value. Assignment expressions thus
cannot be used for any kind of resulting value. As such, the following is
invalid:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
y: Int = x: Int = 4 -- compile-time error
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Because names are immutable in Apollo, any name must be defined in the same
line that it is declared. Declaring a name without a value is not allowed, and
so the following is invalid:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
y: Int -- compile-time error
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### Value Expressions

Every other Apollo expression has a value. Essentially, if the expression is
not an assignment, it has a value and will be evaluated. Because Apollo imposes
functional purity, a value expression can in effect can always be replaced with
its value.

A block of code implicitly returns the value of its last expression. As such, a
`return` keyword (as in the C programming language) is unnecessary. For
example, any function will return the value of its final expression. Similarly,
any branch of a conditional statement will return its final expression if that
branch is evaluated.

#### Blocks

A block is a value expression consisting of one or more expressions and is delimited by curly braces. The value of a block is the value of its last expression. Therefore, the last expression of a block must be a value expression. The previous expressions, however, can be a combination of assignment and value expressions.

Blocks can be used to declare local-scope auxiliary values or functions. Consider the following two versions of a function that computes the surface area of a cylinder:

##### One: using a block:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cylinderArea: (r: Int, h: Int) -> Int = {
    sideArea: Int = 2 * r * pi * h
    baseArea: Int = 2 * r * r
    sideArea + 2 * baseArea
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##### Two: without a block:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cylinderArea: (r: Int, h: Int) -> Int = 2 * r * pi * h + 2 * (2 * r * r)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Both versions produce the same result, but the first one is arguably more
readable, modular, and easy to reason about.

Control Flow
------------

Conditionals are statements that perform different computations depending on whether a programmer-specified boolean condition evaluates to `True` or `False` in a sequence of such conditions.

A conditional expression is a series of one or more `case` statements, followed by an `otherwise` statement. A case statement must be followed by a single parenthesis-enclosed expression that must evaluate to a `Bool`. The first `case` statement whose condition evaluates to `True` will be evaluated and the value it returns will be the value for the entire conditional statement. If no preceding case condition evaluates to `True`, the `otherwise` expression will be evaluated. Note that this means if multiple case conditions evaluate to `True`, only the first of these case expressions will be evaluated.

TODO: explain example

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
foo: Int = case (1 > 2) { 1 }
           case (False) { 2 }
           case (True)  { 3 }
           case (True)  { 4 }
           otherwise    { 5 }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Program Structure and Scope
---------------------------

A program in Apollo is made up of one or more valid statements. A program
begins in a main function which needs to be defined for a program to compile.
This main function can execute statements and call functions.

### Block Scoping

An execution block is a list of statements enclosed between the starting curly
brace `{` and the respective closing brace `}`. Blocks can be nested. Names
defined within a block have a scope limited to that block. Names defined in a
scope within which a given block is contained are accessible within that block
as long as that block does not redefine the same name. If the given block
redefines a name defined in a containing block, the former shadows the latter
(i.e. the former is unaffected by the later, but it is not accessible within
the block in which the later is accessible).

### Function Scoping

Since a function is effectively a parameterized name assigned to a block, the
body of a function follows the rules of block scoping. This means that a
function defined within a function creates a new block inside the parent
function's block.

Hello World
-----------

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aMajor: Atom = Chord([`a5, `c#5, `e5])

back: Part = [aMajor, bMinor, eMajor, aMajor]

lead: Part = [(`a5, 10), (`b5, 10), (`c4, 10)]

main: Music = Multi([lead, back])
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Type Hierarchy
--------------

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data Music
	= SingleTrack Part
| MultiTrack [Part]

data Part
	= Part [Atom]

data Atom
	= Note Pitch Duration
	| Rest Duration
	| Chord [Pitch] Duration

data Rhythm
	= Rhythm [Duration]

type Pitch = Int

type Duration = Int
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Syntax Features & "Syntactic Sugar"
-----------------------------------

### Lists

**TODO**

### Duration Macros

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Regular expression: \\[0-9]+\.?

Syntax

\<denominator>

\1		64
\2		32
\3		21	(floor)
\4		16
\8
\16
\32
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Pitch Macros

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Regular expression: `[a-g](#|b)?[0-9]

`a5		a in the 5th octave

a: Pitch = `a5
b: Pitch = 123

n: Note = (a, \3)

vs

n1: Note = `a64
n1: Note = a:\64
n2: Note = a5:\1
n3: Note = 9

p: Phrase = Phrase([(`a5,\1), `b5:3, `c5:3}

p: Phrase = [(`a5, 1), (`b5, 3), (`c5, 3)]
p: Phrase = {`a5, `b5, `c5} <= 1

this could be cool hehe: |`a5, `b5, `c5|

c: Chord = [(`c5, 3), (`e5, 2), (`g5, 1)]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Grammar
-------

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Program     : Expressions
Expressions : Expression
            | Expression Expressions
Expression  : Assignment
            | Value
            | Block
            | Conditional
Assignment  : Declaration '=' Expression
Declaration : ID ':' ID
            | ID ':' FnType
FnType      : '(' Params ')' '->' ID
Params      : Declaration
            | Declaration ',' Params
Value       : NUM
Block       : '{' Expressions '}'
Conditional : CASE '(' Expression ')' Expression OTHERWISE Expression
            | CASE '(' Expression ')' Expression Conditional
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

