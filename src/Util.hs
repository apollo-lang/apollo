
--------------------------------------------------------------------------
-- Util: utilities for type and data construction in Apollo expressions
--------------------------------------------------------------------------

module Util (
  def
, param
, lambda
, parsePitch
, parseDuration
, makeMusic
, randomRange
) where

import System.Random
import Text.Regex.Posix
import Expr
import Type

def :: Id -> [Param] -> Type -> Expr -> Expr
def i p t e = Def i (TFunc (snd p') t) (VLam (fst p') e)
  where
    p' = unpackParams p

param :: Id -> [Param] -> Type -> Param
param i p t = Param i (TFunc (snd p') t)
  where
    p' = unpackParams p

lambda :: [Param] -> Type -> Expr -> Expr
lambda p t e = VTLam (snd p') t (fst p') e
  where
    p' = unpackParams p

unpackParams :: [Param] -> ([Id], [Type])
unpackParams = unzip . map (\(Param i t) -> (i, t))

unpackList :: Expr -> [Expr]
unpackList (VList exprs) = exprs
unpackList _             = error "Expected expression list"

toPitch :: Expr -> Pitch
toPitch (VPitch p) = Pitch p
toPitch (VInt i)   = Pitch $ i `mod` 128
toPitch _          = error "Expected VInt or VPitch"

toDuration :: Expr -> Duration
toDuration (VDuration p) = Duration p
toDuration (VInt i)      = Duration i
toDuration _             = error "Expected VInt or VPitch"

makeAtom :: Expr -> Atom
makeAtom (VAtom Nil (VDuration d))            = AtomRest $ Rest (Duration d)
makeAtom (VAtom p@(VPitch _) d@(VDuration _)) = AtomNote $ Note (toPitch p) (toDuration d)
makeAtom (VAtom pitches d@(VDuration _))      = AtomChord $ Chord (map toPitch $ unpackList pitches) (toDuration d)
makeAtom _                                    = error "Expected note, chord or rest"

makeMusic :: Expr -> Music
makeMusic (VList m) = Music $ map (map makeAtom . unpackList)  m
makeMusic _         = error "Error: name to export to midi must be of type Music"

pitchClass :: String -> Int
pitchClass "C" = 0
pitchClass "D" = 2
pitchClass "E" = 4
pitchClass "F" = 5
pitchClass "G" = 7
pitchClass "A" = 9
pitchClass "B" = 11
pitchClass _   = error "Invalid pitch class"

accidental :: String -> Int
accidental "b" = -1
accidental "#" = 1
accidental _   = 0

pitchHeight :: Int -> Int -> Int -> Int
pitchHeight pc acc octave = (pc + acc) + 12 * (octave + 1)

parsePitch :: String -> Int
parsePitch s = case matchPitch s of
    [] -> error "Invalid pitch (no match)"
    ms -> case head ms of
        [_, pc, acc, octave] -> pitchHeight
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

randomRange :: Int -> Int -> Int
randomRange a b = head . randomRs (a, b) . mkStdGen $ 13128930232

