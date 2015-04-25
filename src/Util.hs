module Util
    ( define
    , construct
    , pitchClass
    , accidental
    , pitchHeight
    , parsePitch
    , matchPitch
    , parseDuration
    , matchDuration
    ) where

import Text.Regex.Posix
import Expr

-- TODO: use Error monad instead of `error`

-- define: Coerce an Int to a Pitch or Duration datatype where appropriate

define :: Id -> Type -> Expr -> Expr
define i (TData "Pitch") (VInt p)
    = Def i (TData "Pitch") (VPitch $ Pitch $ p `mod` 128)
define i (TData "Duration") (VInt d)
    | d < 1 || d > 256 = error "Duration out of range [1, 256]"
    | otherwise        = Def i (TData "Duration") (VDuration (Duration d))
define i t e = Def i t e

unpackInt :: Expr -> Int
unpackInt (VInt i)                  = i
unpackInt (VPitch (Pitch p))        = p
unpackInt (VDuration (Duration d))  = d
unpackInt _                         = error "Expected int, pitch, or duration"

unpackList :: Expr -> [Expr]
unpackList (VList exprs) = exprs
unpackList _ = error "Expected expression list"

construct :: Type -> [Expr] -> Expr
construct (TData "Pitch") [pitch]
    = VPitch $ Pitch $ unpackInt pitch
construct (TData "Note") [pitch, dur]
    = VNote $ Note (Pitch $ unpackInt pitch) (Duration $ unpackInt dur)
construct (TData "Chord") [pitches, dur]
    = VChord $ Chord (map (Pitch . unpackInt) $ unpackList pitches) (Duration $ unpackInt dur)
construct _ _ = error "Syntax error"

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

parsePitch :: String -> Pitch
parsePitch s = case matchPitch s of
    [] -> error "Invalid pitch (no match)"
    ms -> case head ms of
        [_, pc, acc, octave] ->
            Pitch $ pitchHeight
                (pitchClass pc)
                (accidental acc)
                (read octave :: Int)
        _ -> error "Invalid pitch (invalid match)"

matchPitch :: String -> [[String]]
matchPitch s = s =~ "([A-G])(b|#)?([0-9])"

parseDuration :: String -> Duration
parseDuration s = case matchDuration s of
    [] -> error "Invalid duration (no match)"
    ms -> case head ms of
        [_, dur, dot] -> case dot of
            "." -> Duration $ 64 `div` (read dur :: Int) * 3 `div` 2
            _   -> Duration $ 64 `div` (read dur :: Int)
        _ -> error "Invalid duration (invalid match)"

matchDuration :: String -> [[String]]
matchDuration s = s =~ "\\\\([0-9]+)(\\.?)"

