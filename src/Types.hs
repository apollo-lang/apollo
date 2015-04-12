module Types where

import Text.Regex.Posix -- for parsing short-hand notation

-- Primitive types
data Pitch 
    = Pitch { pitch :: Int }
    deriving (Show)

data Duration 
    = Duration { duration :: Int }
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
    = Part { atoms :: [Atom] }
    deriving (Show)

data Music 
    = Music { parts :: [Part] }
    deriving (Show)

pitchClass :: String -> Int
pitchClass "C"  = 0
pitchClass "D"  = 2
pitchClass "E"  = 4
pitchClass "F"  = 5
pitchClass "G"  = 7
pitchClass "A"  = 9
pitchClass "B"  = 11
pitchClass _    = error "Invalid pitch class"

accidental :: String -> Int
accidental "b"  = -1
accidental "#"  = 1
accidental _    = 0

pitchHeight :: Int -> Int -> Int -> Int
pitchHeight pc acc octave = (pc + acc) + 12 * (octave + 1)

matchPitch :: String -> [[String]]
matchPitch s = s =~ "`([A-G])(b|#)?([0-9])"

parsePitch :: String -> Pitch
parsePitch s = case matchPitch s of
    [] -> error "Invalid pitch"         -- no match
    ms -> case head ms of
        [_, pc, acc, octave] -> 
            Pitch $ pitchHeight 
                (pitchClass pc) 
                (accidental acc) 
                (read octave :: Int)
        _ -> error "Invalid pitch"      -- invalid match

matchDuration :: String -> [[String]]
matchDuration s = s =~ "\\\\([0-9]+)(\\.?)"

parseDuration :: String -> Duration
parseDuration s = case matchDuration s of
    [] -> error "Invalid duration"      -- no match
    ms -> case head ms of
        [_, dur, dot] -> case dot of
            "." -> Duration $ 64 `div` (read dur :: Int) * 3 `div` 2
            _   -> Duration $ 64 `div` (read dur :: Int)
        _ -> error "Invalid duration"   -- invalid match

