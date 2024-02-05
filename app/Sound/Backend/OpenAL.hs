{-# LANGUAGE BlockArguments #-}
module Sound.Backend.OpenAL where

import Control.Monad ( replicateM, when, void )
import Data.Int
import Data.Maybe ( fromJust )
import Foreign.Marshal.Array
import Foreign.Ptr

import Control.Concurrent ( threadDelay )
import Control.Concurrent.STM
import Control.Monad.State
import qualified Sound.OpenAL as AL
import Sound.OpenAL ( ($=) )

import Sound.Backend.Environment


data DataBuffer = DataBuffer AL.Buffer (Ptr Int16)

data ALState = ALState { currentBuffer :: Int
                       , internalBuffer :: [Int16]
                       , buffers :: [DataBuffer]
                       , source :: AL.Source }


allocBuffer :: Int -> IO DataBuffer
allocBuffer bufSize = do
    alBuffer <- AL.genObjectName
    pArray <- mallocArray bufSize
    return (DataBuffer alBuffer pArray)


alMain :: TQueue Int16 -> Environment -> StateT ALState IO ()
alMain audioQueue env = do
    alState <- get
    liftIO $ threadDelay 100_000
    let (ALState { currentBuffer, internalBuffer, buffers, source }) = alState
    let (Environment { bufSize, bufCount, sampleRate }) = env
    liftIO $ putStrLn ((show $ length internalBuffer) ++ " samples in buffer")

    nProcessedBuffers <- AL.get $ AL.buffersProcessed source
    liftIO $ putStrLn ("processed buffers: " ++ (show nProcessedBuffers))
    when (nProcessedBuffers > 0) do
        void $ AL.unqueueBuffers source nProcessedBuffers

    nq <- AL.get $ AL.buffersQueued source
    let nQueuedBuffers = fromIntegral nq
    liftIO $ putStrLn ("queued buffers: " ++ (show nq))
    let iBufLength = length internalBuffer

    if (nQueuedBuffers < bufCount && iBufLength > bufSize) then do
        let (DataBuffer alBuffer pArray) = buffers !! currentBuffer

        -- put n=bufSize samples from internal buffer into an OpenAL buffer
        let samples = if (iBufLength > bufSize)
            then take bufSize internalBuffer
            else internalBuffer ++ (replicate (bufSize - iBufLength) 0)

        liftIO $ pokeArray pArray samples
        let bufSizeBytes = bufSize * 2  -- Int16 = 2 bytes
        let region = AL.MemoryRegion pArray (fromIntegral bufSizeBytes)
        AL.bufferData alBuffer $= AL.BufferData region AL.Mono16 sampleRate
        
        -- play the newly queued audio if not already playing
        AL.queueBuffers source [alBuffer]
        srcState <- AL.get $ AL.sourceState source
        when (srcState /= AL.Playing) (AL.play [source])

        put alState { currentBuffer = (currentBuffer + 1) `mod` bufCount
                    , internalBuffer = drop bufSize internalBuffer }
    else do
        samples <- liftIO $ atomically $ flushTQueue audioQueue
        put alState { internalBuffer = internalBuffer ++ samples }
    
    alMain audioQueue env


runAL :: Environment -> (Environment -> StateT ALState IO ()) -> IO ()
runAL env program = do
    let (Environment { bufSize, bufCount, sampleRate }) = env
    let streamAttributes = [ AL.Frequency sampleRate
                           , AL.MonoSources 1
                           , AL.StereoSources 0 ]

    maybeDevice <- AL.openDevice Nothing  -- select the "preferred" device
    let device = fromJust maybeDevice

    maybeContext <- AL.createContext device streamAttributes
    AL.currentContext $= maybeContext

    dBuffers <- replicateM bufCount (allocBuffer bufSize)
    alSource <- AL.genObjectName

    let alState = ALState { buffers = dBuffers
                          , currentBuffer = 0
                          , internalBuffer = []
                          , source = alSource }
    
    evalStateT (program env) alState
