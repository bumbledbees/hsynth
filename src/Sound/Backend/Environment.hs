module Sound.Backend.Environment where

import Data.Ratio ( (%) )

import Data.Time.Clock


data Environment = Environment { bufCount :: Int
                               , bufSize :: Int
                               , initialTime :: UTCTime
                               , sampleRate :: Int }


initEnv :: UTCTime -> Environment
initEnv t = Environment { bufCount = 3
                        , bufSize = bs
                        , initialTime = t
                        , sampleRate = 44_100 }
    where rate = 44_100
          -- round up to next multiple of 64 for memory alignment
          s = 10 :: Int
          bs = ceiling(rate % (s * 64)) * 64  -- ~(1 / s) seconds
