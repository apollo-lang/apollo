module Midi where

import Codec.Midi

-- Wrapper functions

notePair chan pitch vel dur = [(0, NoteOn chan pitch vel), (dur, NoteOff chan pitch vel)]

noteTrack chan pitch vel dur = notePair chan pitch vel dur ++ [(0, TrackEnd)]

-- Multi-track MIDI

chordMidi = Midi {  fileType = MultiTrack, 
                    timeDiv  = TicksPerBeat 24,
			        tracks   = cMajorTracks }

-- Multi-track Chords

chordTracks chordTones = [noteTrack 0 (60 + x) 80 96 | x <- chordTones]

cMajor = [0, 4, 7, 12]

cMajorTracks = chordTracks cMajor


-- Single-track MIDI

myTrack :: Int -> Ticks -> [Int] -> [(Ticks, Message)]
myTrack root dur xs = notePairs ++ [(0, TrackEnd)]
    where notes     = [notePair 0 (root + offset) 80 dur | offset <- xs]
          notePairs = foldr (\x y -> x ++ y) [] notes 

mySequence track =
    Midi { fileType = SingleTrack,
           timeDiv = TicksPerBeat 24,
	   tracks = [track] }

-- Sequence 1: Fibonacci

fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibSeq n = [fib i | i <- [0..n]]

fibTrack = myTrack 60 12 (fibSeq 10)

fibPhrase = mySequence fibTrack

-- Sequence 2: Collatz

collatz 1 = [1]
collatz n
    | n `mod` 2 == 0 = [n] ++ collatz (n `div` 2)
    | otherwise      = [n] ++ collatz (3 * n + 1)

collatzTrack = myTrack 60 12 (collatz 100)

collatzPhrase = mySequence collatzTrack

export myMidi = exportFile "test.mid" myMidi

-- Some tests

chordTest = export $ chordMidi cMajorTracks

fibTest = export fibPhrase

collatzTest = export collatzPhrase
