-- | Main.hs
-- Entry point for the Pure Concurrent Data Cache Simulator
-- Demonstrates: IO monad, program composition, modular design

module Main where

import DataTypes
import Utils
import Processing
import IOHandler
import System.IO

-- | Main entry point
main :: IO ()
main = do
    -- Set buffering for better user experience
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin LineBuffering
    
    -- Display welcome screen
    displayWelcome
    
    -- Ask user for mode selection
    putStrLn "Select mode:"
    putStrLn "  1. Run automated demo"
    putStrLn "  2. Interactive mode"
    putStr "\nChoice (default 1): "
    choice <- getLine
    
    -- Initialize cache
    cache <- initializeCache
    
    -- Run selected mode
    case choice of
        "2" -> do
            displayHelp
            interactiveLoop cache
        _ -> runDemo cache
