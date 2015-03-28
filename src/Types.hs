module Types where

-- Primitive types
data Pitch 
        = Pitch Int
        deriving (Show)

data Duration 
        = Duration Int
        deriving (Show)

-- Derived types
data Note 
        = Note Pitch Duration
        deriving (Show)


data Chord 
        = Chord [Pitch]
        deriving (Show)

data Rest 
        = Rest Duration
        deriving (Show)

data Atom 
        = AtomNote Note 
        | AtomChord Chord
        | AtomRest Rest
        deriving (Show)

data Rythm 
        = Rythm [Duration]
        deriving (Show)

data Part 
        = Part [Atom]
        deriving (Show)

data Music 
        = Music [Part]
        deriving (Show)

