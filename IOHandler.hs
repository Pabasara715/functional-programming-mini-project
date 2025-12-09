-- | IOHandler.hs
-- Handles all I/O operations (impure functions isolated here)
-- Demonstrates: Separation of pure and impure code, IO monad

module IOHandler where

import DataTypes
import Utils
import Processing
import System.IO
import Control.Monad (when)
import Text.Printf (printf)
import qualified Data.Map.Strict as Map

-- | Display welcome message
displayWelcome :: IO ()
displayWelcome = do
    putStrLn "\n=========================================="
    putStrLn "   Pure Concurrent Data Cache Simulator"
    putStrLn "=========================================="
    putStrLn ""
    putStrLn "A functional programming demonstration of"
    putStrLn "thread-safe caching with immutable state"
    putStrLn ""

-- | Display help message
displayHelp :: IO ()
displayHelp = do
    putStrLn "\nAvailable Commands:"
    putStrLn "  get <key>           - Lookup a value by key"
    putStrLn "  put <key> <value>   - Insert/update a key-value pair"
    putStrLn "  evict <key>         - Manually evict a key"
    putStrLn "  stats               - Display cache statistics"
    putStrLn "  clear               - Clear all cache entries"
    putStrLn "  help                - Show this help message"
    putStrLn "  quit/exit           - Exit the program"
    putStrLn ""

-- | Display cache statistics
displayStats :: CacheStats -> IO ()
displayStats stats = do
    putStrLn "\n--- Cache Statistics ---"
    printf "Total Hits:     %d\n" (totalHits stats)
    printf "Total Misses:   %d\n" (totalMisses stats)
    printf "Hit Rate:       %.2f%%\n" (hitRate stats * 100)
    printf "Current Size:   %d/%d\n" (currentSize stats) (maxCapacity stats)
    putStrLn "------------------------\n"

-- | Display cache contents
displayCache :: CacheState -> IO ()
displayCache cache = do
    putStrLn "\n--- Cache Contents ---"
    if Map.null (entries cache)
    then putStrLn "(empty)"
    else mapM_ putStrLn (formatCache cache)
    putStrLn "----------------------\n"

-- | Display operation result
displayResult :: CacheResult -> IO ()
displayResult result = case result of
    Hit val -> putStrLn $ "[HIT] " ++ val
    Miss -> putStrLn "[MISS] Key not found"
    Updated -> putStrLn "[OK] Cache updated successfully"
    Evicted key -> putStrLn $ "[EVICTED] " ++ key
    Error msg -> putStrLn $ "[ERROR] " ++ msg

-- | Read and parse user input
readCommand :: IO (Maybe CacheOperation)
readCommand = do
    putStr "> "
    hFlush stdout
    input <- getLine
    case words input of
        ["help"] -> displayHelp >> return Nothing
        [] -> return Nothing
        _ -> return $ parseCommand input

-- | Main interactive loop
-- Demonstrates: IO monad, recursion, state threading
interactiveLoop :: CacheState -> IO ()
interactiveLoop cache = do
    maybeOp <- readCommand
    case maybeOp of
        Nothing -> interactiveLoop cache  -- Invalid command or help
        Just Quit -> do
            putStrLn "\nThank you for using the cache simulator!"
            putStrLn "Goodbye!\n"
        Just GetStats -> do
            displayStats (calculateStats cache)
            interactiveLoop cache
        Just op -> do
            let (result, newCache) = processOperation op cache
            displayResult result
            when (op == Clear) $ putStrLn "Cache cleared."
            interactiveLoop newCache

-- | Load test data from commands
loadTestData :: CacheState -> IO CacheState
loadTestData cache = do
    putStrLn "\nLoading test data..."
    let testOps = 
            [ Insert "user:1001" "John Doe"
            , Insert "user:1002" "Jane Smith"
            , Insert "user:1003" "Bob Johnson"
            , Insert "product:201" "Laptop"
            , Insert "product:202" "Mouse"
            ]
    let results = processOperations testOps cache
    let finalCache = snd $ last results
    putStrLn $ "Loaded " ++ show (length testOps) ++ " test entries."
    return finalCache

-- | Run automated demo
runDemo :: CacheState -> IO ()
runDemo cache = do
    putStrLn "\n--- Running Automated Demo ---\n"
    
    -- Load test data
    cache1 <- loadTestData cache
    displayCache cache1
    
    -- Perform lookups
    putStrLn "Performing lookups..."
    let (r1, cache2) = lookupCache "user:1001" cache1
    displayResult r1
    
    let (r2, cache3) = lookupCache "user:9999" cache2
    displayResult r2
    
    -- Insert with eviction
    putStrLn "\nInserting with eviction (cache full)..."
    let (r3, cache4) = insertCache "product:203" "Keyboard" cache3
    displayResult r3
    
    displayCache cache4
    displayStats (calculateStats cache4)
    
    putStrLn "\n--- Demo Complete ---\n"
    
    -- Start interactive mode
    putStrLn "Entering interactive mode...\n"
    displayHelp
    interactiveLoop cache4

-- | Initialize cache based on user input
initializeCache :: IO CacheState
initializeCache = do
    putStr "Enter cache capacity (default 5): "
    hFlush stdout
    capacityInput <- getLine
    let cap = if null capacityInput then 5 else read capacityInput :: Int
    
    putStr "Select eviction policy (1=LRU, 2=FIFO, default=LRU): "
    hFlush stdout
    policyInput <- getLine
    let pol = case policyInput of
                "2" -> FIFO
                _ -> LRU
    
    putStrLn $ "\nCache initialized with capacity " ++ show cap ++ " and " ++ show pol ++ " policy.\n"
    return $ createCache cap pol
