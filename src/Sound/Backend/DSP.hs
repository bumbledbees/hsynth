module Sound.Backend.DSP where

import Data.Int
import Data.Ratio

import Data.Time.Clock

import Sound.Notes
import Sound.Backend.Util ( maxIntNBound )


data WaveFunction = Sine | Triangle | Square | Sawtooth | Noise
    deriving (Show, Eq)


getWaveFunction :: WaveFunction -> (Float -> Float)
getWaveFunction waveFn = case waveFn of
    Sine -> sin
    Square -> signum . sin
    _ -> (\_ -> 0)


-- round time to the nearest interval of (1 / sampleRate).
quantizeTime :: NominalDiffTime -> Int -> Ratio Int
quantizeTime time rate = n % rate
    where a = fromEnum time
          b = 1_000_000_000_000  -- NominalDiffTime has resolution of 1e12
          n = round((a * rate) % b)
          

sampleF32toS16 :: Float -> Int16
sampleF32toS16 sample = truncate((maxIntNBound 16) * sample)


sampleWaveform :: (Float -> Float) -> NoteState -> Octave -> Float -> Float
sampleWaveform waveFn (NoteOn note) octave time = waveFn(time * interval) 
    where pitch = noteToPitch note octave
          interval = pitch * 2 * pi

sampleWaveform _ NoteOff _ _ = 0 


genSamples :: Int -> WaveFunction -> NoteState -> Octave -> NominalDiffTime ->
              NominalDiffTime -> [Float]
genSamples rate waveFn ns octave start end =
    map (sampleWaveform func ns octave . fromRational . toRational)
        [start', step .. end']
    where func = getWaveFunction waveFn
          start' = quantizeTime start rate
          step = start' + (1 % rate)
          end' = quantizeTime end rate - (1 % rate)
