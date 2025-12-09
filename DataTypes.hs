{-# LANGUAGE DeriveGeneric #-}

-- | DataTypes.hs
-- Defines all algebraic data types (ADTs) for the cache simulator
-- Demonstrates: ADTs, type safety, immutability

module DataTypes where

import GHC.Generics (Generic)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- | Represents a cache key (String for simplicity)
type CacheKey = String

-- | Represents a cache value (String for simplicity)
type CacheValue = String

-- | Timestamp for LRU tracking
type Timestamp = Int

-- | Cache entry containing value and last access time
data CacheEntry = CacheEntry
    { value :: CacheValue
    , lastAccessed :: Timestamp
    } deriving (Show, Eq, Generic)

-- | Cache eviction policy (Algebraic Data Type)
data EvictionPolicy 
    = LRU  -- Least Recently Used
    | FIFO -- First In First Out
    deriving (Show, Eq, Generic)

-- | Cache state containing all cache data
-- Pure, immutable data structure
data CacheState = CacheState
    { entries :: Map CacheKey CacheEntry
    , capacity :: Int
    , currentTime :: Timestamp
    , policy :: EvictionPolicy
    , hitCount :: Int
    , missCount :: Int
    } deriving (Show, Eq, Generic)

-- | Result of a cache operation
data CacheResult 
    = Hit CacheValue        -- Cache hit with value
    | Miss                  -- Cache miss
    | Updated               -- Cache updated successfully
    | Evicted CacheKey      -- Key evicted from cache
    | Error String          -- Error occurred
    deriving (Show, Eq)

-- | Cache operation commands
data CacheOperation
    = Lookup CacheKey
    | Insert CacheKey CacheValue
    | Evict CacheKey
    | GetStats
    | Clear
    | Quit
    deriving (Show, Eq)

-- | Cache statistics for monitoring
data CacheStats = CacheStats
    { totalHits :: Int
    , totalMisses :: Int
    , hitRate :: Double
    , currentSize :: Int
    , maxCapacity :: Int
    } deriving (Show, Eq)
