-- | Processing.hs
-- Core cache operations - all pure functions
-- Demonstrates: Pure functions, immutability, state transformation

module Processing where

import DataTypes
import Utils
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- | Lookup a key in the cache
-- Pure function - returns new state and result
lookupCache :: CacheKey -> CacheState -> (CacheResult, CacheState)
lookupCache key cache = 
    case Map.lookup key (entries cache) of
        Just entry -> 
            -- Cache hit: update access time and hit count
            let updatedEntry = entry { lastAccessed = currentTime cache }
                updatedEntries = Map.insert key updatedEntry (entries cache)
                newCache = incrementTime $ cache 
                    { entries = updatedEntries
                    , hitCount = hitCount cache + 1
                    }
            in (Hit (value entry), newCache)
        Nothing -> 
            -- Cache miss: increment miss count
            let newCache = incrementTime $ cache 
                    { missCount = missCount cache + 1 }
            in (Miss, newCache)

-- | Insert a key-value pair into the cache
-- Pure function with eviction logic
insertCache :: CacheKey -> CacheValue -> CacheState -> (CacheResult, CacheState)
insertCache key val cache
    | not (isValidKey key) = (Error "Invalid key", cache)
    | not (isValidValue val) = (Error "Invalid value", cache)
    | otherwise = 
        let cache' = incrementTime cache
            now    = currentTime cache'
            newEntry = CacheEntry
                { value = val
                , insertedAt = now
                , lastAccessed = now
                }
        in if Map.member key (entries cache')
           then -- Key exists, update it
               let updatedEntries = Map.insert key newEntry (entries cache')
                   newCache = cache' { entries = updatedEntries }
               in (Updated, newCache)
           else -- Key doesn't exist, check capacity
               if isFull cache'
               then -- Need to evict
                   case findKeyToEvict cache' of
                       Nothing -> (Error "Cache full, no key to evict", cache')
                       Just evictKey -> 
                           let entriesAfterEvict = Map.delete evictKey (entries cache')
                               entriesAfterInsert = Map.insert key newEntry entriesAfterEvict
                               newCache = cache' { entries = entriesAfterInsert }
                           in (Evicted evictKey, newCache)
               else -- Space available, just insert
                   let updatedEntries = Map.insert key newEntry (entries cache')
                       newCache = cache' { entries = updatedEntries }
                   in (Updated, newCache)

-- | Find key to evict based on policy
-- Pure function demonstrating pattern matching on ADTs
findKeyToEvict :: CacheState -> Maybe CacheKey
findKeyToEvict cache = case policy cache of
    LRU -> findLRUKey (entries cache)
    FIFO -> findFIFOKey (entries cache)

-- | Evict a specific key from cache
-- Pure function
evictKey :: CacheKey -> CacheState -> (CacheResult, CacheState)
evictKey key cache = 
    if Map.member key (entries cache)
    then let updatedEntries = Map.delete key (entries cache)
             newCache = cache { entries = updatedEntries }
         in (Evicted key, newCache)
    else (Error "Key not found", cache)

-- | Clear all entries from cache
-- Pure function - returns new empty cache
clearCache :: CacheState -> CacheState
clearCache cache = cache 
    { entries = Map.empty
    , hitCount = 0
    , missCount = 0
    }

-- | Process a single cache operation
-- Pure function demonstrating pattern matching
processOperation :: CacheOperation -> CacheState -> (CacheResult, CacheState)
processOperation op cache = case op of
    Lookup key -> lookupCache key cache
    Insert key val -> insertCache key val cache
    Evict key -> evictKey key cache
    Clear -> (Updated, clearCache cache)
    _ -> (Error "Unsupported operation", cache)

-- | Process multiple operations sequentially
-- Demonstrates: Recursion, fold pattern
processOperations :: [CacheOperation] -> CacheState -> [(CacheResult, CacheState)]
processOperations ops initialCache = 
    scanl processOp (Updated, initialCache) ops
    where
        processOp (_, cache) op = processOperation op cache

-- | Batch insert operations
-- Demonstrates: Higher-order functions, fold
batchInsert :: [(CacheKey, CacheValue)] -> CacheState -> CacheState
batchInsert pairs initialCache = 
    foldl (\cache (k, v) -> snd $ insertCache k v cache) initialCache pairs

-- | Get cache statistics
getStatistics :: CacheState -> CacheStats
getStatistics = calculateStats
