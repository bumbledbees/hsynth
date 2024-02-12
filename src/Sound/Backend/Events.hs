module Sound.Backend.Events where

import Data.Time.Clock

import Sound.Notes


data Event
    = NoteSet NoteState
    | OctaveSet Octave
    | OctaveUp
    | OctaveDown
    | Noop
    deriving (Show, Eq)

data TimedEvent = TimedEvent UTCTime Event
    deriving (Show, Eq)
