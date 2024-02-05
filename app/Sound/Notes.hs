module Sound.Notes where


data Note = C | Db | D | Eb | E | F | Gb | G | Ab | A | Bb | B
    deriving (Show, Ord, Eq, Enum)

data NoteState = NoteOn Note | NoteOff
    deriving (Show, Eq)

type Octave = Int


-- A4 == 440
referencePitch :: Float
referencePitch = 440.0

-- 2 ^ 1/12
twelfthRoot :: Float
twelfthRoot = 1.059_463_094_359_295


noteToPitch :: Note -> Octave -> Float
noteToPitch note oct = referencePitch * (twelfthRoot ^^ deltaSemitones)
    where deltaSemitones = (oct * 12 + (fromEnum note)) - (48 + (fromEnum A))