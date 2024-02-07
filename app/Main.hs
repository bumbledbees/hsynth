module Main where

import Control.Concurrent ( forkOS )

import Sound.Backend
import UI


main :: IO ()
main = do
    b <- initBackend
    let (Backend { environment = env, eventQueue = eq }) = b

    backendThread <- forkOS $ runBackend b
    putStrLn $ "Backend thread: " ++ (show backendThread)
    runUI env eq

    return ()
