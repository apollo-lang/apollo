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

Apollo is a functional programming language for algorithmic music composition.
Apollo is intended to be usable by a programmer with knowledge of basic
functional constructs and no prior experience with music creation. The
fine-details of synthesizing music are abstracted such that familiar
programming types like integers can be interpreted by the compiler as musical
sequences. At the same time, more experienced musicians can directly manipulate
note and chord types while leveraging Apollo's programming constructs to create
novel compositions. In effect, Apollo empowers the programmer to hear the sound
of algorithms and the musician to compose in code.  An Apollo source program
produces as output a MIDI file, which is a standardized way to store a musical
piece.

### Why is it called Apollo?

Apollo combines the power of source code, or Apollonian logic, with the art of
music, the domain of the god Apollo. The logo is derived from a combination of
the f in functional and the f-hole opening in the body of a cello.

Functional Constructs
---------------------

Since the functional programming paradigm encompasses a number of different
programming constructs, it is useful to describe the manner in which Apollo is
a functional language. 

### First-class functions

Functions are first-class citizens in Apollo. This means that functions can be:
 * passed as function parameters
 * returned as values by other functions

### Immutable data

Apollo variables are not like C variables, which can be manipulated and be
changed during the execution of a program. A value can be bound to a type and
a variable only once. This allows programs to be more safe and easy to reason
about. The return value of a function for a given input will always be the
same, since there are is data mutation, or side effects.

Lexical Elements
----------------

Apollo has five types of tokens: keywords, identifiers, constants, operators
and separators.

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

### Aliased Types

#### Pitch

A type alias for an `Int`, ranging from 0 to 127, interpreted as a musical
pitch, or the height of a note on a musical staff.

##### Short-hand notation

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

|    | c   | c# db | d   | d# eb | e   | f   | f# gb | g   | g# ab | a   | a# bb | b   |
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
| 10 | 120 | 121   | 122 | 123   | 124 | 125 |       |     |       |     |       |     |

This notation is inspired by the way notes are defined in MIDI. It can be
described by the regular expression

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
`[a-g](#|b)?[0-9]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##### Duration

A type alias for an `Int` ranging from 1 to 256, which indicates a multiple of
the smallest possible duration -- a 64th note or a 64th of a beat in 4/4 time.

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
\1		-- Whole note.          Equivalent to 64
\2		-- Half note.           Equivalent to 32
\4		-- Quarter note.        Equivalent to 16
\8      -- Eigth note.          Equivalent to 8
\16     -- Sixteenth note.      Equivalent to 4
\32     -- Thirtysecond note.   Equivalent to 2
\64     -- Sixtyfourth note.    Equivalent to 1
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

 * `Atom`: a polymorphic type indicating a musical sound-unit. Instances of
    `Atom` can be either a `Note`, a `Chord`, or a `Rest`. For example:

	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	n: Atom = Note(`a5, \8)
	c: Atom = Chord([`a5, `c#5, `e5], \4)
	r: Atom = Rest(\4)
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 * `Note`: an instance of the Atom type consisting of a `Pitch` and a
   `Duration`. For example:

	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	n: Atom = Note(`a5, \4)
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 * `Chord`: an instance of the `Atom` type consisting of a list of `Pitch`es
    and a `Duration`. For example:

	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	c: Atom = Chord([`a5, `c#5, `e5], \4)
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 * `Rest`: an instance of the `Atom` type consisting of `Duration`. A `Rest`
    indicates a space in which no notes are played. For example:

	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	r: Atom = Rest(\4)
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 * `Rhythm`: a list of `Duration`s. For example:

	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	r: Rhythm = Rhythm([\4, \8, \8])
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 * `Part`: a is a list of `Atom`s. This is useful for distinguishing
   separate but simultaneously-occurring lines of music. For example:

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

### Type System

Apollo types are polymorphic. This means that a type can take on different
shapes. Apollo types adhere to the following rules:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Pitch   : Int

Duration: Int

Rhythm	: Rhythm [Duration]

Atom    : Note Pitch Duration
        | Rest Duration
        | Chord [Pitch] Duration

Part    : Part [Atom]

Music   : Music [Part]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The items on the left side are types; the items on the right side are instances
of those types, together with their components.

Consider the `Atom` type. The `Atom` type has three instances with names
`Note`, `Rest`, and `Chord`. The `Note` instance, for example, takes two
parameters: the first one with type Pitch and the second one with type
Duration.

Types like Pitch and Duration are just different names for the Int type.

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
side.

#### `expression < expression`

If the expressions both have integral type, then `<` yields `True` if and only
if the expression on the left side is smaller than the expression on the right
side.

#### `expression >= expression`

If the expressions both have integral type, then `>=` yields `True` if and only
if the expression on the left side is larger than or equal to the expression on
the right side.

#### `expression <= expression`

If both expressions have integral type, then `<=` yields `True` if and only if
the expression on the left side is smaller than or equal to the expression on
the right side.

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
applicable to the assignment operator.

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

Functions and values are declared using the same syntax. Functions, however,
have different types -- they take one or more types and return another type.
Like mathematical functions, they map elements in one or more sets to an
element in another set. Let's define a function that takes an integer x and
returns its square.
    
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
square: (x: Int) -> Int = x * x
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A function consists of a declaration and definition. The declaration of square is
    
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
square: (x: Int) -> Int
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

which reads as "square is a function that takes an Int whose identifier is x and returns an Int."

The definition is whatever is to the right side of the assignment operator:
    
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
x * x
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that declarations cannot exist on their own; they require a definition.

Functions in Apollo can be passed as parameters to other functions. Example:
        
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
twice: (f: (n: Int) -> Int, x: Int) = f(f(x))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The twice function takes a function f and applies it twice to the argument x.
Now we can use our two functions to declare a new function, pow4, which takes
an integer x and returns x^4.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pow4: (x: Int) -> Int = twice(square, x)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Functions can be recursive, that is, they can call themselves. Consider the
following function for computing the factorial of an integer n:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
factorial: (n: Int) -> Int = 
    case (n == 0) 1 otherwise n * factorial(n - 1)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As in all recursive functions, we need a base case to prevent infinite
recursive calls. For this we use conditional statements. The functions reads as
"the factorial of n is 1 if n is 0, otherwise it is n times the factorial
of n - 1."
 
Looping
-------

Looping can be simulated using recursion. To create a subroutine that should
loop, a recursive function can be defined. A simple example of a recursive
loop:

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
y: Int = x: Int = 4     -- invalid
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Because names are immutable in Apollo, any name must be defined in the same
line that it is declared. Declaring a name without a value is not allowed, and
so the following is invalid:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
y: Int                  -- invalid
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

A block is a value expression consisting of one or more expressions and is
delimited by curly braces. The value of a block is the value of its last
expression. Therefore, the last expression of a block must be a value
expression. The previous expressions, however, can be a combination of
assignment and value expressions.

Blocks can be used to declare local-scope auxiliary values or functions.
Consider the following two versions of a function that computes the surface
area of a cylinder:

##### One: using a block:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cylinderArea: (r: Int, h: Int) -> Int = {
    sideArea: Int = 2 * pi * r * h
    baseArea: Int = 2 * pi * r * r
    sideArea + 2 * baseArea
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##### Two: without a block:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cylinderArea: (r: Int, h: Int) -> Int = 2 * pi * r * h + 2 * (2 * pi * r * r)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Both versions produce the same result, but the first one is arguably more
readable, modular, and easy to reason about.

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
foo: Int = case (1 > 2) { 1 }
           case (False) { 2 }
           case (True)  { 3 }
           case (True)  { 4 }
           otherwise    { 5 }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Here foo is an integer whose value is determined by a case statement. The first two case
statements evaluate to `False` and their return value is ignored. The third statement is the 
first one to evaluate to `True` and foo is therefore assigned the value `3`.

Program Structure and Scope
---------------------------

A program in Apollo is made up of one or more valid statements. A program
begins in a main variable, which is of type Music and is required for a program
to compile. For example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
main: Music = Music([Part([Note(`a5, \4)])])
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The program will compile to a MIDI file containing a single note -- an a in the
fifth octave with a quarter-note duration.

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

Grammar
-------

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Program     : Expressions

Expressions : Expression
            | Expression Expressions

Expression  : Assignment
            | Value
            | UnOp Value
            | Value BinOp Value
            | Block
            | Conditional

Assignment  : Declaration '=' Expression

Declaration : ID ':' Type
            | ID ':' FnType

Type        : Int
            | Bool 
            | Pitch
            | Duration
            | Atom
            | Music
            | FnType

FnType      : '(' Params ')' '->' ID

Params      : Declaration
            | Declaration ',' Params

Value       : '(' Value ')'
            | NUM
            | Constructor '(' Vals ')'

Vals        : Value
            | Value ',' Vals

UnOp        : '-' 
            | '!'

BinOp       : '+'
            | '-'
            | '*'
            | '/'
            | '%'
            | '='
            | '=='
            | '!=' 
            | '<'
            | '>'
            | '<=' 
            | '>='
            | '&&'
            | '||'

Block       : '{' Expressions '}'

Conditional : CASE '(' Expression ')' Expression OTHERWISE Expression
            | CASE '(' Expression ')' Expression Conditional
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

