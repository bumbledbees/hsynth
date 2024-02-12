module Sound.Backend.OpenAL where

import Control.Monad ( replicateM, when, void )
import Data.Int
import Data.Maybe ( fromJust )
import Foreign.Marshal.Array

import Control.Concurrent ( threadDelay )
import Control.Concurrent.STM
import Control.Monad.State
import qualified Sound.OpenAL as AL
import Sound.OpenAL ( ($=) )

import Sound.Backend.Environment


data DataBuffer = DataBuffer AL.Buffer (AL.MemoryRegion Int16)

data ALState = ALState { currentBuffer :: Int
                       , internalBuffer :: [Int16]
                       , buffers :: [DataBuffer]
                       , source :: AL.Source }


allocBuffer :: Int -> IO DataBuffer
allocBuffer bufSize = do
    alBuffer <- AL.genObjectName
    pArray <- mallocArray bufSize
    let bufSizeBytes = bufSize * 2  -- Int16 = 2 bytes
    let memRegion = AL.MemoryRegion pArray (fromIntegral bufSizeBytes)
    return (DataBuffer alBuffer memRegion)


alMain :: TQueue Int16 -> Environment -> StateT ALState IO ()
alMain audioQueue env = do
    let (Environment { bufSize, bufCount, sampleRate }) = env
    liftIO $ threadDelay $ 1_000_000 `div` (sampleRate `div` 2)

    alState <- get
    let (ALState { currentBuffer, internalBuffer, buffers, source }) = alState

    nProcessedBuffers <- AL.get $ AL.buffersProcessed source
    -- liftIO $ putStrLn $ "processed buffers: " ++ show nProcessedBuffers
    when (nProcessedBuffers > 0) do
        void $ AL.unqueueBuffers source nProcessedBuffers

    nq <- AL.get $ AL.buffersQueued source
    let nQueuedBuffers = fromIntegral nq
    -- liftIO $ putStrLn ("queued buffers: " ++ (show nq))
    let iBufLength = length internalBuffer
    -- liftIO $ putStrLn ((show $ iBufLength) ++ " samples in buffer")

    if nQueuedBuffers < bufCount && iBufLength > bufSize then do
        let (DataBuffer alBuffer memRegion) = buffers !! currentBuffer
        let (AL.MemoryRegion pArray _) = memRegion

        -- put n=bufSize samples from internal buffer into an OpenAL buffer
        let samples = take bufSize internalBuffer
        liftIO $ pokeArray pArray samples
        let bytes = AL.BufferData memRegion AL.Mono16 (fromIntegral sampleRate)
        AL.bufferData alBuffer $= bytes

        -- play the newly queued audio if not already playing
        -- liftIO $ putStrLn ("Queueing buffer " ++ (show currentBuffer))
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
    let streamAttributes = [ AL.Frequency (fromIntegral sampleRate)
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
