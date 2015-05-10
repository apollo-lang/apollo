module Util
    ( def
    , define
    , param
    , parsePitch
    , parseDuration
    , makeMusic
    ) where

import Text.Regex.Posix
import Expr
import Type
-- import Error

-- TODO: use Error monad instead of `error`


-- define: For TFunc, store param names with body as FnBody type
define :: Id -> Type -> Expr -> Expr
define i t e = case (t, e) of 
    (TPitch, VInt n) -> Def i t (VPitch (n `mod` 128))
    (TDuration, VInt n) -> Def i t (VDuration n)
    _                -> Def i t e 

def :: Id -> ([Param], Type) -> Expr -> Expr
def iden (params, retType) body = Def iden (TFunc (snd params') retType) (VLam (fst params') body)
  where params' = unzip $ unpackParam params

param :: Id -> ([Param], Type) -> Param
param iden (params, t) = Param iden (TFunc (snd params') t)
  where params' = unzip $ unpackParam params

unpackParam :: [Param] -> [(Id, Type)]
unpackParam = map (\(Param i t) -> (i, t))


unpackList :: Expr -> [Expr]
unpackList (VList exprs) = exprs
unpackList _ = error "Expected expression list"

makeAtom :: Expr -> Atom
makeAtom (VAtom (VPitch p) (VDuration d))   = AtomNote $ Note (Pitch p) (Duration d)
makeAtom (VAtom Nil (VDuration d))          = AtomRest $ Rest (Duration d)
makeAtom (VAtom pitches (VDuration d))      = AtomChord $ Chord (map (\(VPitch p) -> (Pitch p)) $ unpackList pitches) (Duration d)
makeAtom _                                  = error "Expected note, chord or rest"

makeMusic :: Expr -> Music
makeMusic (VList m) = Music $ map ((map makeAtom) . unpackList)  m
makeMusic _          = error "bug: expected VMusic"

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

parsePitch :: String -> Int
parsePitch s = case matchPitch s of
    [] -> error "Invalid pitch (no match)"
    ms -> case head ms of
        [_, pc, acc, octave] ->
                pitchHeight
                (pitchClass pc)
                (accidental acc)
                (read octave :: Int)
        _ -> error "Invalid pitch (invalid match)"

matchPitch :: String -> [[String]]
matchPitch s = s =~ "([A-G])(b|#)?([0-9])"

parseDuration :: String -> Int
parseDuration s = case matchDuration s of
    [] -> error "Invalid duration (no match)"
    ms -> case head ms of
        [_, dur, dot] -> case dot of
            "." -> 64 `div` (read dur :: Int) * 3 `div` 2
            _   -> 64 `div` (read dur :: Int)
        _ -> error "Invalid duration (invalid match)"

matchDuration :: String -> [[String]]
matchDuration s = s =~ "\\\\([0-9]+)(\\.?)"

