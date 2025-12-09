# Technical Report: Pure Concurrent Data Cache Simulator

**Course:** Functional Programming  
**Project Type:** Mini Project  
**Group Members:**  
- EG/2020/3935: Galpayage G. D. T. G  
- EG/2020/4252: Vihanga V. M. B. P  
- EG/2020/4030: Kuanayaka H.P  
- EG/2020/4247: Udayanthika K.D.S.  
**Date:** December 2025

---

## 1. Problem Statement and Industrial Motivation

### 1.1 Problem Statement

Modern distributed systems require high-performance caching mechanisms to reduce latency and improve throughput. Traditional cache implementations using mutable state and locks face several challenges:

- **Concurrency bugs**: Race conditions, deadlocks, and data corruption
- **Testing complexity**: Non-deterministic behavior makes unit testing difficult
- **Maintenance burden**: Hidden state mutations lead to unexpected bugs
- **Scalability limits**: Lock contention becomes a bottleneck under high load

This project implements a **pure functional cache simulator** that eliminates these problems by treating all state transformations as pure, immutable operations. The cache supports standard operations (lookup, insert, evict) with configurable eviction policies (LRU, FIFO) while maintaining thread-safety without explicit synchronization.

### 1.2 Industrial Relevance

**Backend Systems & APIs:**
- Web servers cache session data, database queries, and API responses
- Example: An e-commerce platform caching product catalog data to reduce database load

**Distributed Systems:**
- Microservices architectures use distributed caches for service discovery and configuration
- Example: Netflix's EVCache serving billions of requests per day

**Data Processing Pipelines:**
- ETL systems cache intermediate results to avoid recomputation
- Example: Apache Spark caching RDDs for iterative algorithms

**Content Delivery Networks (CDNs):**
- Edge servers cache static assets with sophisticated eviction policies
- Example: Cloudflare caching website assets across global data centers

**Real-time Systems:**
- Financial trading platforms cache market data for microsecond latency
- IoT systems cache sensor readings for efficient aggregation

---

## 2. Functional Design

### 2.1 Type Signatures

#### Core Data Types
```haskell
-- Cache state (immutable)
data CacheState = CacheState
    { entries :: Map CacheKey CacheEntry
    , capacity :: Int
    , currentTime :: Timestamp
    , policy :: EvictionPolicy
    , hitCount :: Int
    , missCount :: Int
    }

-- Cache entry with metadata
data CacheEntry = CacheEntry
    { value :: CacheValue
    , lastAccessed :: Timestamp
    }

-- Eviction policies (ADT)
data EvictionPolicy = LRU | FIFO

-- Operation results (ADT)
data CacheResult 
    = Hit CacheValue 
    | Miss 
    | Updated 
    | Evicted CacheKey 
    | Error String
```

#### Main Functions

**Cache Initialization:**
```haskell
createCache :: Int -> EvictionPolicy -> CacheState
```
Creates an empty cache with specified capacity and eviction policy.

**Lookup Operation:**
```haskell
lookupCache :: CacheKey -> CacheState -> (CacheResult, CacheState)
```
Searches for a key in cache. Returns Hit with value if found, Miss otherwise. Updates access time on hit.

**Insert Operation:**
```haskell
insertCache :: CacheKey -> CacheValue -> CacheState -> (CacheResult, CacheState)
```
Inserts or updates a key-value pair. Triggers eviction if cache is full.

**Eviction Logic:**
```haskell
findKeyToEvict :: CacheState -> Maybe CacheKey
findLRUKey :: Map CacheKey CacheEntry -> Maybe CacheKey
findFIFOKey :: Map CacheKey CacheEntry -> Maybe CacheKey
```
Determines which key to evict based on policy (LRU or FIFO).

**Statistics:**
```haskell
calculateStats :: CacheState -> CacheStats
getStatistics :: CacheState -> CacheStats
```
Computes cache performance metrics (hit rate, size, etc.).

**Parallel Operations:**
```haskell
parallelLookups :: [CacheKey] -> CacheState -> [CacheResult]
batchInsert :: [(CacheKey, CacheValue)] -> CacheState -> CacheState
```
Demonstrates concurrent operations possible with pure functions.

### 2.2 Architecture Overview

The system follows a layered architecture:

```
┌─────────────────────────────────────┐
│         Main.hs (Entry)            │
│    - Program orchestration         │
│    - Mode selection                │
└──────────────┬──────────────────────┘
               │
┌──────────────▼──────────────────────┐
│      IOHandler.hs (Impure)         │
│    - User interaction              │
│    - Display functions             │
│    - Interactive loop              │
└──────────────┬──────────────────────┘
               │
┌──────────────▼──────────────────────┐
│     Processing.hs (Pure Logic)     │
│    - lookupCache                   │
│    - insertCache                   │
│    - evictKey                      │
│    - Parallel operations           │
└──────────────┬──────────────────────┘
               │
┌──────────────▼──────────────────────┐
│  Utils.hs (Pure Utilities)         │
│    - createCache                   │
│    - calculateStats                │
│    - findLRUKey / findFIFOKey      │
└──────────────┬──────────────────────┘
               │
┌──────────────▼──────────────────────┐
│   DataTypes.hs (Type Definitions)  │
│    - CacheState                    │
│    - EvictionPolicy (ADT)          │
│    - CacheResult (ADT)             │
└────────────────────────────────────┘
```

**Design Principles:**
1. **Separation of Concerns**: Pure logic (Processing, Utils) separate from I/O (IOHandler)
2. **Type Safety**: All operations are statically typed with meaningful ADTs
3. **Immutability**: State transformations return new states rather than mutating
4. **Composability**: Small, focused functions compose into complex behaviors

---

## 3. Functional Programming Concepts Applied

### 3.1 Pure Functions

**Definition:** Functions with no side effects that always return the same output for the same input.

**Implementation in Project:**
```haskell
lookupCache :: CacheKey -> CacheState -> (CacheResult, CacheState)
lookupCache key cache = 
    case Map.lookup key (entries cache) of
        Just entry -> 
            let updatedEntry = entry { lastAccessed = currentTime cache }
                updatedEntries = Map.insert key updatedEntry (entries cache)
                newCache = incrementTime $ cache 
                    { entries = updatedEntries, hitCount = hitCount cache + 1 }
            in (Hit (value entry), newCache)
        Nothing -> (Miss, incrementTime $ cache { missCount = missCount cache + 1 })
```

**Benefits:**
- **Testable**: Easy to unit test with predictable inputs/outputs
- **Thread-safe**: Multiple threads can call with same cache state safely
- **Debuggable**: No hidden state to track

### 3.2 Immutability

**Definition:** Data structures cannot be modified after creation.

**Implementation:**
```haskell
insertCache :: CacheKey -> CacheValue -> CacheState -> (CacheResult, CacheState)
-- Never mutates original cache, always returns new cache
```

**Benefits:**
- **No race conditions**: Impossible to have concurrent modification bugs
- **Time-travel debugging**: Old states remain accessible
- **Persistent data structures**: Multiple versions coexist efficiently

### 3.3 Algebraic Data Types (ADTs)

**Definition:** Type-safe sum types representing distinct possibilities.

**Implementation:**
```haskell
data EvictionPolicy = LRU | FIFO

data CacheResult 
    = Hit CacheValue 
    | Miss 
    | Updated 
    | Evicted CacheKey 
    | Error String

-- Pattern matching ensures all cases handled
processResult :: CacheResult -> String
processResult (Hit val) = "Found: " ++ val
processResult Miss = "Not found"
processResult Updated = "Success"
processResult (Evicted key) = "Evicted: " ++ key
processResult (Error msg) = "Error: " ++ msg
```

**Benefits:**
- **Compile-time guarantees**: Compiler ensures exhaustive pattern matching
- **Self-documenting**: Types clearly express domain concepts
- **Refactoring safety**: Adding new cases causes compiler errors where updates needed

### 3.4 Recursion

**Definition:** Functions that call themselves instead of using loops.

**Implementation:**
```haskell
interactiveLoop :: CacheState -> IO ()
interactiveLoop cache = do
    operation <- readCommand
    case operation of
        Just Quit -> putStrLn "Goodbye!"
        Just op -> do
            let (result, newCache) = processOperation op cache
            displayResult result
            interactiveLoop newCache  -- Recursive call with new state
        Nothing -> interactiveLoop cache
```

**Benefits:**
- **Natural for functional style**: Fits immutable data paradigm
- **Tail-call optimization**: GHC optimizes tail recursion to constant stack
- **Easier reasoning**: Each iteration is independent

### 3.5 Higher-Order Functions

**Definition:** Functions that take functions as parameters or return functions.

**Implementation:**
```haskell
-- map: applies function to all elements
formatCache :: CacheState -> [String]
formatCache cache = map formatEntry (Map.toList (entries cache))

-- foldl: accumulates results (batch insert)
batchInsert :: [(CacheKey, CacheValue)] -> CacheState -> CacheState
batchInsert pairs initialCache = 
    foldl (\cache (k, v) -> snd $ insertCache k v cache) initialCache pairs

-- parMap: parallel map for concurrent processing
parallelLookups :: [CacheKey] -> CacheState -> [CacheResult]
parallelLookups keys cache = parMap rdeepseq (\key -> fst $ lookupCache key cache) keys
```

**Benefits:**
- **Code reuse**: Generic operations work on any data
- **Abstraction**: Hide implementation details
- **Composability**: Functions combine like building blocks

### 3.6 Type Safety

**Definition:** Compiler enforces type correctness at compile-time.

**Implementation:**
```haskell
type CacheKey = String      -- Distinct type for keys
type CacheValue = String    -- Distinct type for values
type Timestamp = Int        -- Distinct type for time

-- Function signatures enforce correct types
insertCache :: CacheKey -> CacheValue -> CacheState -> (CacheResult, CacheState)
-- Cannot accidentally swap key and value
```

**Benefits:**
- **Early error detection**: Type errors caught before runtime
- **Documentation**: Types serve as specifications
- **Refactoring confidence**: Type system catches inconsistencies

### 3.7 Monadic I/O

**Definition:** I/O operations sequenced using the IO monad, separating pure and impure code.

**Implementation:**
```haskell
-- Pure computation
calculateStats :: CacheState -> CacheStats  -- Pure function

-- Impure I/O
displayStats :: CacheStats -> IO ()         -- I/O action
displayStats stats = do
    putStrLn "Statistics:"
    print stats
```

**Benefits:**
- **Referential transparency**: Pure functions remain pure
- **Testability**: Pure logic testable without I/O
- **Clear boundaries**: Type system enforces separation

### 3.8 Pattern Matching

**Definition:** Deconstructing data types to handle different cases.

**Implementation:**
```haskell
findKeyToEvict :: CacheState -> Maybe CacheKey
findKeyToEvict cache = case policy cache of
    LRU -> findLRUKey (entries cache)
    FIFO -> findFIFOKey (entries cache)

processOperation :: CacheOperation -> CacheState -> (CacheResult, CacheState)
processOperation op cache = case op of
    Lookup key -> lookupCache key cache
    Insert key val -> insertCache key val cache
    Evict key -> evictKey key cache
    Clear -> (Updated, clearCache cache)
```

**Benefits:**
- **Exhaustiveness checking**: Compiler warns of missing cases
- **Readability**: Logic flow is explicit
- **Maintainability**: Adding new variants is straightforward

### 3.9 Parallel Processing

**Definition:** Executing operations concurrently on multiple cores.

**Implementation:**
```haskell
import Control.Parallel.Strategies (parMap, rdeepseq)

parallelLookups :: [CacheKey] -> CacheState -> [CacheResult]
parallelLookups keys cache = 
    parMap rdeepseq (\key -> fst $ lookupCache key cache) keys
```

**Benefits:**
- **Free parallelism**: Pure functions safe to parallelize without locks
- **Scalability**: Automatically utilizes multiple cores
- **No data races**: Immutability eliminates concurrency bugs

---

## 4. Expected Outputs and Testing

### 4.1 Sample Execution Scenarios

**Scenario 1: Basic Operations**
```
Input:  put user:1 Alice
Output: ✓ Cache updated successfully

Input:  get user:1
Output: ✓ HIT: Alice

Input:  get user:999
Output: ✗ MISS: Key not found
```

**Scenario 2: Eviction (Cache Full)**
```
Capacity: 3, Policy: LRU

Operations:
1. put key1 value1  →  Cache: [key1]
2. put key2 value2  →  Cache: [key1, key2]
3. put key3 value3  →  Cache: [key1, key2, key3]  (full)
4. get key1         →  Cache: [key2, key3, key1]  (key1 moved to end)
5. put key4 value4  →  Cache: [key3, key1, key4]  (key2 evicted - LRU)

Output: ⚠ Evicted key: key2
```

**Scenario 3: Statistics Tracking**
```
Operations: 5 inserts, 3 hits, 2 misses

--- Cache Statistics ---
Total Hits:     3
Total Misses:   2
Hit Rate:       60.00%
Current Size:   5/5
------------------------
```

### 4.2 Testing Strategy

**Unit Testing (Pure Functions):**
```haskell
-- Test in GHCi
let cache = createCache 3 LRU
let (r1, c1) = insertCache "k1" "v1" cache
-- r1 == Updated, size c1 == 1

let (r2, c2) = lookupCache "k1" c1
-- r2 == Hit "v1", hitCount c2 == 1

let (r3, c3) = lookupCache "k999" c2
-- r3 == Miss, missCount c3 == 1
```

**Integration Testing:**
Test complete workflows through automated demo mode.

**Concurrency Testing:**
```haskell
-- Verify parallel lookups produce same results as sequential
let keys = ["key1", "key2", "key3"]
let results = parallelLookups keys cache
-- All results should be consistent with cache state
```

---

## 5. Possible Extensions

### 5.1 Feature Extensions

1. **TTL (Time To Live):**
   ```haskell
   data CacheEntry = CacheEntry 
       { value :: CacheValue
       , lastAccessed :: Timestamp
       , expiresAt :: Maybe Timestamp  -- Add expiration
       }
   ```

2. **Additional Eviction Policies:**
   - LFU (Least Frequently Used)
   - Random eviction
   - Size-based eviction

3. **Persistent Storage:**
   - Write-through cache (immediate persistence)
   - Write-back cache (lazy persistence)
   - Snapshot/restore functionality

4. **Distributed Cache:**
   - Consistent hashing for sharding
   - Replication for fault tolerance
   - Eventual consistency protocols

5. **Advanced Features:**
   - Cache warming strategies
   - Namespace support for multi-tenancy
   - Bulk operations (mget, mset)
   - Transaction support with rollback

### 5.2 Technical Extensions

1. **Performance Optimizations:**
   - Use STM (Software Transactional Memory) for true concurrency
   - Implement cache warming strategies
   - Add bloom filters for negative lookups

2. **Monitoring & Observability:**
   - Export metrics in Prometheus format
   - Logging with structured data
   - Distributed tracing integration

3. **API Interfaces:**
   - RESTful HTTP API
   - gRPC service
   - WebSocket for real-time updates

---

## 6. Discussion: Why FP Improves Reliability and Concurrency

### 6.1 Reliability Improvements

**Predictable Behavior:**
Pure functions with immutable state eliminate entire classes of bugs:
- No unexpected state mutations
- No action-at-a-distance effects
- Deterministic execution for same inputs

**Testability:**
```haskell
-- Pure function: easy to test
testLookup = 
    let cache = createCache 5 LRU
        (_, c1) = insertCache "key" "value" cache
        (result, _) = lookupCache "key" c1
    in result == Hit "value"  -- Deterministic, no mocking needed
```

**Error Handling:**
Types make error cases explicit:
```haskell
data CacheResult = ... | Error String

-- Compiler forces handling of error cases
case processOperation op cache of
    Error msg -> handleError msg  -- Must handle or get warning
    result -> processResult result
```

### 6.2 Concurrency Improvements

**Thread-Safety Without Locks:**

Traditional approach (Java):
```java
class Cache {
    private Map<String, String> cache;
    
    synchronized String get(String key) {  // Lock required
        return cache.get(key);
    }
}
```

Functional approach (Haskell):
```haskell
lookupCache :: CacheKey -> CacheState -> (CacheResult, CacheState)
-- No locks needed - immutable state makes it thread-safe
```

**Parallel Processing:**
```haskell
-- Safe to run in parallel - no shared mutable state
parallelLookups keys cache = parMap rdeepseq lookupLogic keys
```

**Scalability:**
- No lock contention bottlenecks
- Automatic parallelization by GHC runtime
- Linear scalability with cores (for embarrassingly parallel tasks)

**Real-World Impact:**
- **Correctness**: Impossible to have race conditions or deadlocks
- **Performance**: No lock overhead, better cache utilization
- **Maintenance**: Easier to reason about concurrent code
- **Testing**: Concurrent tests are deterministic and reproducible

### 6.3 Industrial Case Studies

**WhatsApp (Erlang/OTP):**
- Handles 2 million connections per server using functional, immutable message passing
- Zero downtime deployments through hot code swapping

**Jane Street (OCaml):**
- Processes billions in trades daily with functional programming
- Type safety prevents costly runtime errors

**Facebook (Haskell for Sigma):**
- Anti-abuse system using Haskell for reliable rule processing
- Immutable rules enable safe concurrent evaluation

---

## 7. Conclusion

This project demonstrates that **functional programming is not just an academic exercise** but a practical approach for building reliable, concurrent systems. By implementing a cache simulator using pure functions and immutable data structures, we achieved:

✓ **Thread-safety without explicit synchronization**  
✓ **Predictable, testable code**  
✓ **Composable, maintainable architecture**  
✓ **Natural parallelism**  
✓ **Type-safe operations**  

The cache simulator showcases how FP principles—purity, immutability, and strong typing—address real-world challenges in distributed systems, making them ideal for modern backend infrastructure.

---

## 8. References

1. **"Real World Haskell"** by Bryan O'Sullivan - Practical FP patterns
2. **"Purely Functional Data Structures"** by Chris Okasaki - Immutable data structures
3. **Redis Documentation** - Industry-standard cache implementation
4. **"Parallel and Concurrent Programming in Haskell"** by Simon Marlow
5. **GHC User Guide** - Haskell compiler documentation

---


