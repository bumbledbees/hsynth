module Main where

import Data.Int
import Data.Time.Clock

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Builder as B
import Control.Concurrent ( forkOS, threadDelay )
import Control.Concurrent.STM
import Control.Monad.State ( evalStateT, liftIO )

import Sound.Backend
import Sound.Backend.Environment
import Sound.Backend.Events
import Sound.Backend.State
import Sound.Backend.OpenAL hiding ( sampleRate )
import Sound.Notes
import UI


doStateThings :: Environment -> State [Int16]
doStateThings env = do
    let t0 = initialTime env
    _ <- evalState env (TimedEvent t0 (NoteSet (NoteOn C)))

    let t1 = addUTCTime (secondsToNominalDiffTime 0.6) t0
    s1 <- evalState env (TimedEvent t1 (NoteSet (NoteOn D)))

    let t2 = addUTCTime (secondsToNominalDiffTime 0.3) t1
    s2 <- evalState env (TimedEvent t2 (NoteSet (NoteOn E)))

    let t3 = addUTCTime (secondsToNominalDiffTime 0.3) t2
    s3 <- evalState env (TimedEvent t3 (NoteSet (NoteOn F)))

    let t4 = addUTCTime (secondsToNominalDiffTime 0.3) t3
    s4 <- evalState env (TimedEvent t4 (NoteSet (NoteOn G)))

    let t5 = addUTCTime (secondsToNominalDiffTime 0.3) t4
    s5 <- evalState env (TimedEvent t5 (NoteSet (NoteOn A)))

    let t6 = addUTCTime (secondsToNominalDiffTime 0.3) t5
    s6 <- evalState env (TimedEvent t6 (NoteSet (NoteOn B)))

    let t7 = addUTCTime (secondsToNominalDiffTime 0.3) t6
    s7 <- evalState env (TimedEvent t7 (NoteSet (NoteOn B)))

    let t8 = addUTCTime (secondsToNominalDiffTime 0.3) t7
    _  <- evalState env (TimedEvent t8 OctaveUp)
    s8 <- evalState env (TimedEvent t8 (NoteSet (NoteOn C)))

    let t9 = addUTCTime (secondsToNominalDiffTime 1.2) t8
    s9 <- evalState env (TimedEvent t9 Noop)

    let fSamples = s1 ++ s2 ++ s3 ++ s4 ++ s5 ++ s6 ++ s7 ++ s8 ++ s9
    -- liftIO $ putStrLn $ "Samples: " ++ (show samples)

    let maxS16 = 32_767
    let samples = map (round . (* (maxS16))) fSamples

    let sampleToByteString = \s -> B.toLazyByteString $ B.int16LE s
    let sampleData = BS.concat $ map sampleToByteString samples
    liftIO $ BS.writeFile "audio.bin" sampleData

    return samples


main :: IO ()
main = do
    b <- initBackend
    let (Backend { audioQueue = aq, eventQueue = eq, environment = env }) = b
    let (Environment { initialTime = it, sampleRate = sr }) = env

    -- samples <- evalStateT (doStateThings env) (initState it)
    -- atomically $ mapM_ (\s -> writeTQueue aq s) samples
    -- forkOS $ runAL sr (alMain aq)

    forkOS $ runBackend b

    runUI eq

    -- threadDelay 5_000_000
    -- let dt = secondsToNominalDiffTime 3
    -- let t  = (addUTCTime dt it)
    -- atomically $ writeTQueue eq (TimedEvent it (NoteSet (NoteOn A)))
    -- atomically $ writeTQueue eq (TimedEvent t (NoteSet NoteOff))
    -- samples <- atomically $ flushTQueue aq
    -- putStrLn $ show samples
    -- let sampleToByteString = \s -> B.toLazyByteString $ B.floatLE s
    -- let sampleData = BS.concat $ map sampleToByteString samples
    -- BS.writeFile "audio.bin" sampleData

    return ()
