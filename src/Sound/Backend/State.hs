module Sound.Backend.State
    ( State(..), initState, handleEvent
    -- convenience re-export
    , module St ) where

import Control.Monad.State as St hiding ( State )
import Data.Time.Clock

import Sound.Backend.Events
import Sound.Backend.WaveFunction
import Sound.Notes


data State = State { note :: NoteState
                   , octave :: Octave
                   , time :: NominalDiffTime
                   , waveFn :: WaveFunction }
    deriving (Show, Eq)


initState :: State
initState = State { note = NoteOff, octave = 4, waveFn = Square, time = 0 }


handleEvent :: UTCTime -> State -> TimedEvent -> State
handleEvent initialTime st (TimedEvent time' event) =
    case event of
        NoteSet n   -> st' { note = n }
        OctaveSet o -> st' { octave = o }
        OctaveUp    -> st' { octave = octave st + 1 }
        OctaveDown  -> st' { octave = octave st - 1 }
        Noop        -> st'
    where st' = st { time = time' `diffUTCTime` initialTime }
