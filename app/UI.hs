module UI where

import Control.Concurrent ( threadDelay )
import Data.Time.Clock ( getCurrentTime )
import System.IO

import Control.Concurrent.STM
import Graphics.Vty ( (<->), (<|>) )
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as V ( mkVty )

import Sound.Backend.Environment
import Sound.Backend.Events
import Sound.Backend.State
import Sound.Notes


type StatefulIO = StateT State IO


-- https://stackoverflow.com/questions/23068218/haskell-read-raw-keyboard-input
getKey :: IO [Char]
getKey = reverse <$> getKey' "" where
    getKey' chars = do
        char <- getChar
        more <- hReady stdin
        (if more then getKey' else return) (char : chars)


centerImg :: Int -> Int -> V.Image -> V.Image
centerImg width height img = img' where
    img_w = V.imageWidth img
    img_h = V.imageHeight img
    pad_x = (width - img_w) `div` 2
    pad_y = (height - img_h) `div` 2
    img' = V.pad pad_x pad_y pad_x pad_y img


imgFromStrings :: [String] -> V.Image
imgFromStrings = foldl (\img st -> img <-> V.string V.defAttr st) V.emptyImage


renderUI :: V.Vty -> Environment -> State -> IO ()
renderUI vty (Environment { sampleRate, bufSize, bufCount })
         (State { note, octave, waveFn }) = do
    (win_x, win_y) <- V.displayBounds (V.outputIface vty)
    let stateStrings = [ "--- State ---"
                       , "    Note: " ++ show note ++ "  "
                       , "  Octave: " ++ show octave
                       , "Waveform: " ++ show waveFn ]
    let settingsStrings = [ "--- Settings ---"
                          , " Sample Rate: " ++ show sampleRate ++ " Hz"
                          , " Buffer Size: " ++ show bufSize ++ " samples"
                          , "# of buffers: " ++ show bufCount ]
    let img = imgFromStrings stateStrings <|> imgFromStrings settingsStrings
    V.update vty $ V.picForImage $ centerImg win_x win_y img


keyEventMap :: [Char] -> Maybe Event
keyEventMap key = case key of
    "a" -> Just $ NoteSet NoteOff
    "z" -> Just $ NoteSet (NoteOn C)
    "s" -> Just $ NoteSet (NoteOn Db)
    "x" -> Just $ NoteSet (NoteOn D)
    "d" -> Just $ NoteSet (NoteOn Eb)
    "c" -> Just $ NoteSet (NoteOn E)
    "v" -> Just $ NoteSet (NoteOn F)
    "g" -> Just $ NoteSet (NoteOn Gb)
    "b" -> Just $ NoteSet (NoteOn G)
    "h" -> Just $ NoteSet (NoteOn Ab)
    "n" -> Just $ NoteSet (NoteOn A)
    "j" -> Just $ NoteSet (NoteOn Bb)
    "m" -> Just $ NoteSet (NoteOn B)
    "," -> Just $ NoteSet (NoteOn C2)
    "l" -> Just $ NoteSet (NoteOn Db2)
    "." -> Just $ NoteSet (NoteOn D2)
    ";" -> Just $ NoteSet (NoteOn Eb2)
    "/" -> Just $ NoteSet (NoteOn E2)
    "o" -> Just OctaveDown
    "p" -> Just OctaveUp
    _   -> Nothing


uiMain :: V.Vty -> Environment -> TQueue TimedEvent -> StatefulIO ()
uiMain vty env queue = do
    let (Environment { initialTime }) = env
    liftIO $ threadDelay 16_667  -- ~ 1 / 60 seconds

    st <- get
    key <- liftIO getKey
    case keyEventMap key of
        Just event -> do
            -- send event to backend
            t <- liftIO getCurrentTime
            let tEvent = TimedEvent t event
            liftIO $ atomically $ writeTQueue queue tEvent

            -- update internal state, redraw screen
            let st' = handleEvent initialTime st tEvent
            liftIO $ renderUI vty env st'
            put st'

            uiMain vty env queue
        Nothing | key == "q" -> return ()
        Nothing -> uiMain vty env queue

runUI :: Environment -> TQueue TimedEvent -> IO ()
runUI env queue = do
    vty <- V.mkVty V.defaultConfig

    -- vty's native input handling seems to introduce a lot of lag. we should
    -- immediately tell this system to shutdown before starting the UI proper.
    V.shutdownInput (V.inputIface vty)
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False

    renderUI vty env initState
    evalStateT (uiMain vty env queue) initState

    -- normally, we would call V.shutdown here, but it doesn't like that we
    -- called shutdownInput earlier. we shut down the output system manually.
    -- see Vty src/Graphics/Vty.hs (mkVtyFromPair)
    V.releaseDisplay (V.outputIface vty)
    V.releaseTerminal (V.outputIface vty)


runMinimalUI :: Environment -> TQueue TimedEvent -> IO ()
runMinimalUI env queue = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    minimalUI
    where
        minimalUI = do
            let (Environment { initialTime }) = env
            threadDelay 16_667  -- ~ 1 / 60 seconds

            key <- getKey
            case keyEventMap key of
                Just event -> do
                    t <- getCurrentTime
                    atomically $ writeTQueue queue (TimedEvent t event)
                    minimalUI
                Nothing | key == "q" -> return ()
                Nothing -> minimalUI
