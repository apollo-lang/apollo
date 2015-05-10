Apollo Style Guide
===================

When something isn't covered by this guide you should stay consistent with the
code in the other modules. Fork of https://github.com/tibbe/haskell-style-guide

Formatting
----------

### Line Length

Maximum line length is *90 characters*. Occasional exceptions are okay for
readability (see Error.hs) --- use your judgement.

### Indentation

Tabs are illegal. Use spaces for indenting. Indent your code blocks with *2
spaces*. Indent the `where` keyword two spaces from the parent block and write
bindings beginning on the next line in most cases, but exceptions can be made
for highly-nested sub-blocks. Some examples:

```haskell
sayHello :: IO ()
sayHello = do
  name <- getLine
  putStrLn $ greeting name
    where
      greeting name = "Hello, " ++ name ++ "!"

filter :: (a -> Bool) -> [a] -> [a]
filter _ []   = []
filter p (x:xs)
  | p x       = x : filter p xs
  | otherwise = filter p xs
```

### Blank Lines

One blank line between top-level definitions. No blank lines between type
signatures and function definitions. Add one blank line between
functions in a type class instance declaration, cases of a `case` statement, or
pattern-matched function cases if the functions bodies are large and. Use your
judgement.

### Whitespace

Surround binary operators with a single space on either side. Use
your better judgement for the insertion of spaces around arithmetic
operators but always be consistent about whitespace on either side of
a binary operator. Don't insert a space after a lambda.

### Data Declarations

Align the constructors in a data type definition. Example:

```haskell
data Tree a = Branch !a !(Tree a) !(Tree a)
            | Leaf
```

For long type names the following formatting is also acceptable:

```haskell
data HttpException
  = InvalidStatusCode Int
  | MissingContentHeader
```

Format records as follows:

```haskell
data Person = Person
  { firstName :: !String
  , lastName  :: !String
  , age       :: !Int
  } deriving (Eq, Show)
```

### List Declarations

Align the elements in the list. Example:

```haskell
exceptions =
  [ InvalidStatusCode
  , MissingContentHeader
  , InternalServerError
  ]
```

Optionally, you can skip the first newline. Use your judgement.

```haskell
directions = [ North
             , East
             , South
             , West
             ]
```

### Pragmas

Put pragmas in the first line of their source file.

### Hanging Lambdas

You may or may not indent the code following a "hanging" lambda. Use
your judgement. Some examples:

```haskell
bar :: IO ()
bar = forM_ [1, 2, 3] $ \n -> do
      putStrLn "Here comes a number!"
      print n

foo :: IO ()
foo = alloca 10 $ \a ->
      alloca 20 $ \b ->
      cFunction a b
```

### Export Lists

Format export lists as follows:

```haskell
module Data.Set (
  MyType(..)
, empty
, singleton
, member
) where
```

### If-then-else clauses

Generally, guards and pattern matches should be preferred over if-then-else
clauses, where possible. Short cases should usually be put on a single line
(when line length allows it).

Align `if`, `then`, `else`:

```haskell
foo = if ...
      then ...
      else ...

foo2 = bar $ \qux -> if predicate qux
                     then doSomethingSilly
                     else someOtherCode
```

### Case expressions

The alternatives in a case expression can be indented using either of
the two following styles:

```haskell
foobar = case something of
  Just j  -> foo
  Nothing -> bar
```

or as

```haskell
foobar = case something of
           Just j  -> foo
           Nothing -> bar
```

Align the `->` arrows when it helps readability.

Imports
-------

Imports should be grouped in the following order:

1. standard library imports
2. related third party imports
3. local application/library specific imports

Do not put a blank line between each group of imports.

Always use explicit import lists or `qualified` imports for standard and third
party libraries but not for local libraries. This makes the code more robust
against changes in these libraries. Exception: The Prelude.

Comments
--------

### Punctuation

Write proper sentences; start with a capital letter and use proper
punctuation.

### Top-Level Definitions

Every top-level definitions should have a type-signature, but comments are not
strictly necessary. A brief comment may be added if deemed necessary:

```haskell
-- Create a copy of an IORef
clone :: MonadIO m => IORef a -> m (IORef a)
clone e = liftIO (readIORef e >>= newIORef)
```

### End-of-Line Comments

Separate end-of-line comments from the code using 2 spaces. Align
comments for data type definitions. Some examples:

```haskell
data Types = VInt   -- Integer value
           | VBool  -- Boolean value


foo :: Int -> Int
foo n = salt * 32 + 9
  where
    salt = 453645243 -- Magic hash salt.
```

Naming
------

Use camel case (e.g. `functionName`) when naming functions and upper
camel case (e.g. `DataType`) when naming data types.

For readability reasons, don't capitalize all letters when using an
abbreviation. For example, write `HttpServer` instead of
`HTTPServer`. Exception: Two letter abbreviations, e.g. `IO`.

### Modules

Use singular when naming modules e.g. use `Data.Map` and
`Data.ByteString.Internal` instead of `Data.Maps` and
`Data.ByteString.Internals`.

Misc
----

### Point-free style ###

Avoid over-using point-free style. For example, this is hard to read:

```haskell
-- Bad:
f = (g .) . h
```

### Warnings ###

Code should be compilable with `-Wall -Werror`. There should be no
warnings.

### Errors ###

If a total function is not possible, it is often a good idea to add a final
case matching errors. Use the `error` function here (and only here), and add a
descriptive message prefixed with "Bug:":

```haskell
extractValue :: Either ApolloError a -> a
extractValue (Right val) = val
extractValue (Left _)    = error $ "Bug: extractValue called with Left"
```

