module ApolloTypes where

data Music
    = Note
    | Chord
    | Phrase

type Note = Int

type Interval = Int

type Duration = Int

data Chord
    = Chord Note AbstractChord
    | AbstractChord [Interval]
 
data Phrase
    = AbstractPhrase [Interval]
    | Phrase Note AbstractPhrase

data Melody = Melody Note [Interval]

