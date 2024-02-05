module Sound.Backend.Environment where

import Data.Time.Clock


data Environment = Environment { bufCount :: Int
                               , bufSize :: Int
                               , initialTime :: UTCTime
                               , sampleRate :: Float }


initEnv :: UTCTime -> Environment
initEnv t = Environment { bufCount = 3
                        , bufSize = bs
                        , initialTime = t
                        , sampleRate = 44_100 }
    where rate = 44_100
          -- round up to next multiple of 64 for memory alignment
          bs = ceiling((rate / 2) / 64) * 64
