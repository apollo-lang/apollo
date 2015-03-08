Apollo Language Reference Manual
================================

Primitive Data Types
--------------------

- Int
- Bool
- Note (typedef for Int)
- Char
- List

### Working with Lists

Default library:

- map
- reduce / fold
- filter

Operators
---------

### Arithmetic

- `+`
- `-`
- `*`
- `/`
- `%`

### Boolean

- `>`
- `<`
- `==`
- `!=`

Functions
---------

### Definition

    functionName(parameters) -> returnType = value

Example:

    f(x: Int) -> Int = x * x

### Call

Function calls are left associative.

    f(n)

Calling a function returned by a function:

    f(n)(x)

### Pipelines

Declaring a function as a composition of other functions:

    comp(x: Int) -> Int = (f . g . h)

Simultaneous declaration/calling:

    (f . g. h)(x)

Syntax
-------

### Blocks

- Delimited by curly braces: `{...}`
- Value of a block is the value of its last expression

Assigning a function to a block:

    f(x: Int) -> Int = {
        b = 4
        x * b
    }

### Conditionals

    case x = 0
        <expression>
    case x = 1
        <expression>
    case x = 2
        <expression>
    otherwise
        <expression>

### Comments

- C-style comments (?)

#### Single-line

    //

#### Multi-line

    /*
     *
     */

#### Reserved Words

- `Int`
- `Bool`
- `case`
- `or`
- `and`
- `for` (?)

Program structure
-----------------

- Music data type encapsulates all others
- Since a musical piece has a value, an apollo program is equivalent to its main list.

Main list:

    main: Music = a

Tokens / lexing
---------------

- id: `[a-z][A-Za-z0-9]*`

Grammar
-------

    Expression ->
    Assignment -> id ‘=’ Expression
    Func -> id ( params ) ‘->’ Type = Expression
    Block -> Line | { Block }

Play function
-------------

    play :: Play -> Play ()
    play = do
        append
