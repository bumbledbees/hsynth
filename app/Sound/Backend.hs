module Sound.Backend where

import Control.Concurrent ( forkOS, threadDelay )
import Control.Monad ( unless )
import Data.Int
import Data.Maybe ( fromMaybe )
import Data.Time.Clock

import Control.Concurrent.STM

import Sound.Backend.DSP
import Sound.Backend.Environment
import Sound.Backend.Events
import Sound.Backend.State
import Sound.Backend.OpenAL


data Backend = Backend { audioQueue :: TQueue Int16
                       , eventQueue :: TQueue TimedEvent
                       , environment :: Environment }

initBackend :: IO Backend
initBackend = do
    aq <- newTQueueIO
    eq <- newTQueueIO
    it <- getCurrentTime
    return Backend { audioQueue = aq
                   , eventQueue = eq
                   , environment = initEnv it }


backendMain :: Backend -> StateT State IO ()
backendMain b = do
    let (Backend { audioQueue, eventQueue, environment }) = b
    let (Environment { initialTime, sampleRate }) = environment
    liftIO $ threadDelay 8_333  -- ~ 1 / 120 s

    st <- get
    -- liftIO $ putStrLn $ show st

    maybeEvent <- liftIO $ atomically $ tryReadTQueue eventQueue
    currentTime <- liftIO getCurrentTime
    let event = fromMaybe (TimedEvent currentTime Noop) maybeEvent
    let st' = handleEvent initialTime st event
    let (State { time = time' }) = st'

    -- liftIO $ putStrLn $ "Event: " ++ show event
    -- liftIO $ putStrLn $ show initialTime

    let samples = genSamples sampleRate st time'
    unless (null samples) do
        let samples' = map sampleF32toS16 samples
        liftIO $ atomically $ mapM_ (writeTQueue audioQueue) samples'

    put st'
    backendMain b


runBackend :: Backend -> IO ()
runBackend b = do
    let (Backend { audioQueue, environment }) = b
    alThread <- forkOS $ runAL environment (alMain audioQueue)
    putStrLn $ "OpenAL subsystem thread: " ++ show alThread
    evalStateT (backendMain b) initState
