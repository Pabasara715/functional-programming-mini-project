-- | Utils.hs
-- Utility functions for the cache simulator
-- Demonstrates: Pure functions, higher-order functions, function composition

module Utils where

import DataTypes
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List (minimumBy, sortBy)
import Data.Ord (comparing)

-- | Create an empty cache with given capacity and policy
-- Pure function - no side effects
createCache :: Int -> EvictionPolicy -> CacheState
createCache cap pol = CacheState
    { entries = Map.empty
    , capacity = cap
    , currentTime = 0
    , policy = pol
    , hitCount = 0
    , missCount = 0
    }

-- | Calculate cache statistics
-- Pure function demonstrating data transformation
calculateStats :: CacheState -> CacheStats
calculateStats cache = CacheStats
    { totalHits = hitCount cache
    , totalMisses = missCount cache
    , hitRate = if total == 0 then 0.0 else fromIntegral (hitCount cache) / fromIntegral total
    , currentSize = Map.size (entries cache)
    , maxCapacity = capacity cache
    }
    where
        total = hitCount cache + missCount cache

-- | Find the least recently used key
-- Demonstrates: Higher-order functions, recursion through library functions
findLRUKey :: Map CacheKey CacheEntry -> Maybe CacheKey
findLRUKey cache
    | Map.null cache = Nothing
    | otherwise = Just $ fst $ minimumBy (comparing (lastAccessed . snd)) (Map.toList cache)

-- | Find the first inserted key (FIFO)
-- Demonstrates: Pattern matching, recursion
findFIFOKey :: Map CacheKey CacheEntry -> Maybe CacheKey
findFIFOKey cache
    | Map.null cache = Nothing
    | otherwise = Just $ fst $ minimumBy (comparing (insertedAt . snd)) (Map.toList cache)

-- | Check if cache is full
isFull :: CacheState -> Bool
isFull cache = Map.size (entries cache) >= capacity cache

-- | Increment the logical clock
-- Pure function - returns new state instead of mutating
incrementTime :: CacheState -> CacheState
incrementTime cache = cache { currentTime = currentTime cache + 1 }

-- | Format cache entry for display
formatEntry :: (CacheKey, CacheEntry) -> String
formatEntry (key, entry) = 
    key ++ " -> " ++ value entry ++ " (accessed: " ++ show (lastAccessed entry) ++ ")"

-- | Format all cache entries
-- Demonstrates: Higher-order function (map), function composition
formatCache :: CacheState -> [String]
formatCache cache = map formatEntry (Map.toList (entries cache))

-- | Parse a simple command string
-- Demonstrates: Pattern matching, Maybe type for error handling
parseCommand :: String -> Maybe CacheOperation
parseCommand input = case words input of
    ["get", key] -> Just (Lookup key)
    ["put", key, val] -> Just (Insert key val)
    ["evict", key] -> Just (Evict key)
    ["stats"] -> Just GetStats
    ["clear"] -> Just Clear
    ["quit"] -> Just Quit
    ["exit"] -> Just Quit
    _ -> Nothing

-- | Validate cache key
-- Pure predicate function
isValidKey :: CacheKey -> Bool
isValidKey key = not (null key) && length key <= 100

-- | Validate cache value
isValidValue :: CacheValue -> Bool
isValidValue val = not (null val) && length val <= 1000
