module Sound.Backend.State where

import qualified Control.Monad.State as S
import Data.Time.Clock

import Sound.Backend.DSP
import Sound.Backend.Environment
import Sound.Backend.Events
import Sound.Notes


data BackendState = BackendState { note :: NoteState
                                 , octave :: Octave
                                 , waveFn :: WaveFunction
                                 , time :: UTCTime }
    deriving (Show, Eq)

type State = S.StateT BackendState IO


initState :: UTCTime -> BackendState
initState t = BackendState { note = NoteOff
                           , octave = 4
                           , waveFn = Square
                           , time = t }


nextState :: BackendState -> TimedEvent -> BackendState
nextState state (TimedEvent time' event) = 
    case event of
        NoteSet n   -> state' { note = n }
        OctaveSet o -> state' { octave = o }
        OctaveUp    -> state' { octave = (octave state + 1) }
        OctaveDown  -> state' { octave = (octave state - 1) }
        Noop        -> state'
    where state' = state { time = time' }


evalState :: Environment -> TimedEvent -> State [Float]
evalState (Environment { sampleRate, initialTime = t0 }) event =
    S.StateT $ \state -> do
        let (TimedEvent time' _) = event
        let (BackendState { note, octave, waveFn, time }) = state
        let start = diffUTCTime time t0
        let end = diffUTCTime time' t0
        let samples = genSamples sampleRate waveFn note octave start end
        let state' = nextState state event
        return (samples, state')
