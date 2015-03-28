module Types where

data Music
    = Music [Part]

data Part
    = Part [Atom]

data Atom
    = Note Pitch Duration
    | Rest Duration
    | Chord [Pitch] Duration
    deriving Show

data Rhythm
    = Rhythm [Duration]
    deriving Show

type Pitch = Int

type Duration = Int

