module UI where

import Control.Concurrent ( threadDelay )
import Data.Time.Clock ( getCurrentTime )
import System.IO

import Control.Concurrent.STM

import Sound.Backend.Events
import Sound.Notes


sendEvent :: TQueue TimedEvent -> Event -> IO ()
sendEvent queue event = do
    time <- getCurrentTime
    -- putStrLn ("UI Event: " ++ (show event))
    atomically $ writeTQueue queue (TimedEvent time event)


-- https://stackoverflow.com/questions/23068218/haskell-read-raw-keyboard-input
getKey :: IO [Char]
getKey = reverse <$> getKey' ""
    where
        getKey' chars = do
            char <- getChar
            more <- hReady stdin
            (if more then getKey' else return) (char : chars)


uiMain :: TQueue TimedEvent -> IO ()
uiMain queue = do
    threadDelay 22_000
    key <- getKey
    case keyEventMap key of
        Just event -> do
            sendEvent queue event
            runUI queue
        Nothing | key == "q" -> return ()
        Nothing -> runUI queue
    where
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
            "o" -> Just OctaveDown
            "p" -> Just OctaveUp
            _   -> Nothing


runUI :: TQueue TimedEvent -> IO ()
runUI queue = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    uiMain queue
