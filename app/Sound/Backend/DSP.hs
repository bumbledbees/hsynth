module Sound.Backend.DSP where

import Data.Int
import Data.Ratio

import Sound.Notes
import Sound.Backend.Util ( maxIntNBound )


-- round time to the nearest interval of (1 / sampleRate).
quantizeTime :: Rational -> Integer -> Float
quantizeTime time rate = fromRational $ n % rate
    where a = numerator time
          b = denominator time
          n = round((a * rate) % b)


downsample :: Float -> Int16
downsample sample = round((maxIntNBound 16) * sample)


downsampleMany :: [Float] -> [Int16]
downsampleMany samples = map downsample samples


sampleWaveform :: (Float -> Float) -> NoteState -> Octave -> Float -> Float
sampleWaveform waveFn (NoteOn note) octave time = waveFn(time * interval) 
    where pitch = noteToPitch note octave
          interval = pitch * 2 * pi

sampleWaveform _ NoteOff _ _ = 0 


genSamples :: Float -> (Float -> Float) -> NoteState -> Octave -> Rational ->
              Rational -> [Float]
genSamples rate waveFn ns octave start end =
    map (sampleWaveform waveFn ns octave) [start', step .. end']
    where intRate = round rate
          start' = quantizeTime start intRate 
          step = start' + (1 / rate)
          end' = quantizeTime end intRate - (1 / rate)
