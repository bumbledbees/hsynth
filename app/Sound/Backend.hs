module Sound.Backend where

import Control.Concurrent ( forkOS, threadDelay )
import Control.Monad.State ( evalStateT, liftIO )
import Data.Int
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


enqueueAudio :: TQueue Int16 -> [Int16] -> IO ()
enqueueAudio _ [] = return ()

enqueueAudio queue samples = do
    atomically $ mapM_ (\s -> writeTQueue queue s) samples


backendMain :: Backend -> State ()
backendMain b = do
    let (Backend { audioQueue, eventQueue, environment }) = b
    liftIO $ threadDelay 500_000
    maybeEvent <- liftIO $ atomically $ tryReadTQueue eventQueue
    currentTime <- liftIO getCurrentTime
    let event = case maybeEvent of
            Just e -> e
            Nothing -> TimedEvent currentTime Noop
    -- liftIO $ putStrLn (show event)
    samples <- evalState environment event
    liftIO $ enqueueAudio audioQueue (downsampleMany samples)
    backendMain b


runBackend :: Backend -> IO ()
runBackend b = do
    let (Backend { audioQueue, environment }) = b
    let (Environment { initialTime }) = environment
    alThread <- forkOS $ runAL environment (alMain audioQueue)
    putStrLn ("Spawned AL thread: " ++ (show alThread))
    let initialState = initState initialTime
    evalStateT (backendMain b) initialState
