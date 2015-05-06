module Midi where

import Codec.Midi
import Expr

musicToTrack :: Music -> [[(Ticks, Message)]]
musicToTrack (Music m) = foldr (\x y -> x ++ y) [] $ map partToTrack m

midiFromMusic :: Music -> Int -> Midi
midiFromMusic m tempo = Midi 
    { fileType = MultiTrack
    , timeDiv = (TicksPerBeat tempo)
    , tracks = musicToTrack m }

export :: Midi -> String -> IO ()
export themidi filename = exportFile filename themidi

-- best function
exportMusic :: Music -> Int -> String -> IO ()
exportMusic music tempo filename = export (midiFromMusic music tempo) filename

-- returns lenght of longest atom
longestAtom :: Part -> Int
longestAtom (Part p) = maximum $ map sizeOfAtom p
  where
    sizeOfAtom (AtomChord (Chord a _)) = length a 
    sizeOfAtom _ = 1

-- Returns a [[Track]] of length l and takes care of rest padding if Atom is
-- not of length l
appendRests :: Int -> Atom -> [[(Ticks, Message)]]
appendRests l (AtomNote n@(Note (Pitch _) d))
    = noteToTrack n : (replicate (l - 1) (restToTrack (Rest d)))
appendRests l (AtomRest r@(Rest _))
    = restToTrack r : (replicate (l - 1) (restToTrack r))
appendRests l (AtomChord c@(Chord a d))
    = chordToTrack c ++ (replicate (l - (length a)) (restToTrack (Rest d)))

-- Takes a part and outputs [[Track]] with padding using partToTracKHelp
partToTrack :: Part -> [[(Ticks, Message)]]
partToTrack p@(Part atoms) = partToTrackHelp (replicate (longestAtom p) []) atoms

-- Appends all Atoms to a list of tracks with Rest padding
partToTrackHelp :: [[(Ticks, Message)]] -> [Atom] -> [[(Ticks, Message)]]
partToTrackHelp container []
    = zipWith (++) container $ replicate (length container) [(0, TrackEnd)]
partToTrackHelp container (x:xs)
    = (partToTrackHelp (zipWith (++) container (appendRests (length container) x)) xs)

-- Turn Atom into list of Ticks and Messages
atomToTrack :: Atom -> [[(Ticks, Message)]]
atomToTrack (AtomNote n) = [noteToTrack n]
atomToTrack (AtomRest r) = [restToTrack r]
atomToTrack (AtomChord c) = chordToTrack c

-- Takes Chord and outputs a list of Tracks with its notes
chordToTrack :: Chord -> [[(Ticks, Message)]]
chordToTrack (Chord pitches (Duration d)) = chords
  where
    chords = [ckvtToTrack 0 p 60 d  | (Pitch p) <- pitches] -- ++ [(0, TrackEnd)]

-- Takes Note and outputs a Track with the note
noteToTrack :: Note -> [(Ticks, Message)]
noteToTrack (Note (Pitch p) (Duration d)) = ckvtToTrack 0 p 60 d

-- Takes Rest and outputs a Track with the rest
restToTrack :: Rest -> [(Ticks, Message)]
restToTrack (Rest (Duration d)) = ckvtToTrack 0 0 0 d

-- Takes root, duration, pitch list and returns a midi track of the pitches in
-- sequence for duration
nSeqTrack :: [Note] -> [(Ticks, Message)]
nSeqTrack ns = (foldr (\x y -> x ++ y) [] $ map noteToTrack ns)

-- Takes Channel Pitch Velocity Ticks and outputs a Track with the note
-- constructed from them
ckvtToTrack :: Channel -> Key -> Velocity -> Ticks -> [(Ticks, Message)]
ckvtToTrack chan pitch vel dur
    = [(0, NoteOn chan pitch vel), (dur, NoteOff chan pitch vel)]

