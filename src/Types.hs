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
    = Part [Atom]
    deriving (Show)

data Music 
    = Music [Part]
    deriving (Show)

pitchClass :: String -> Int
pitchClass s
    | s == "C"  = 0
    | s == "D"  = 2
    | s == "E"  = 4
    | s == "F"  = 5
    | s == "G"  = 7
    | s == "A"  = 9
    | s == "B"  = 11
    | otherwise = error "Invalid pitch class"

accidental :: String -> Int
accidental s
    | s == "b"  = -1
    | s == "#"  = 1
    | otherwise = 0

pitchHeight :: Int -> Int -> Int -> Int
pitchHeight pc acc octave = (pc + acc) + 12 * (octave + 1)

matchPitch :: String -> [[String]]
matchPitch s = s =~ regex
    where regex = "`([A-G])(b|#)?([0-9])"

parsePitch :: String -> Pitch
parsePitch s = case matchPitch s of
    [] -> error "Syntax error"
    ms -> case head ms of
        [_, pc, acc, octave] -> 
            Pitch 
                (pitchHeight 
                    (pitchClass pc) 
                    (accidental acc) 
                    (read octave :: Int))
        _ -> error "Syntax error"

matchDuration :: String -> [[String]]
matchDuration s = s =~ regex
    where regex = "\\\\([0-9]+)(\\.?)"

parseDuration :: String -> Duration
parseDuration s = case matchDuration s of
    [] -> error "Syntax error"
    ms -> case head ms of
        [_, dur, dot] -> case dot of
            ""  -> Duration $ 64 `div` (read dur :: Int)
            _   -> Duration $ 64 `div` (read dur :: Int) * 3 `div` 2
        _ -> error "Syntax error"
