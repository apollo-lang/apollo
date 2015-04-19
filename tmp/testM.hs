import Types
import Midi

tempo = 24

doe = parsePitch "`C4"
re = parsePitch "`D4"
mi = parsePitch "`E4"
fa = parsePitch "`F4"
sol = parsePitch "`G4"
la = parsePitch "`A4"
si = parsePitch "`B4"

short = Duration 24
mid = Duration 48
long = Duration 96

shortR = Rest short
shortA = AtomRest shortR

doeA = AtomNote (Note doe short)
reA = AtomNote (Note re short)
miA = AtomNote (Note mi short)
faA = AtomNote (Note fa short)
solA = AtomNote (Note sol short)
laA = AtomNote (Note la short)
siA = AtomNote (Note si short)

cI = AtomChord (Chord [mi, sol] mid)
cIV = AtomChord (Chord [fa, la, doe] mid)
cV = AtomChord (Chord [sol, si, re, fa] mid)

melody = Part [doeA, doeA, reA, miA, doeA, miA, reA]
chords = Part [cI, cV, cIV]

song = Music [melody, chords]

main :: IO ()
main = do exportMusic song tempo "test.mid"