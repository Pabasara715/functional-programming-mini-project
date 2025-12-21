# Pure Concurrent Data Cache Simulator

## Group Members
- EG/2020/3935: Galpayage G. D. T. G
- EG/2020/4252: Vihanga V. M. B. P
- EG/2020/4030: Kumanayake H.P
- EG/2020/4247: Udayanthika K.D.S.

**Project Title:** A Pure, Concurrent Data Cache Simulator

---

## Problem Description

### Real-World Scenario
In modern distributed systems and high-performance applications, data caching is essential for reducing latency and improving throughput. Systems like Redis, Memcached, and CDN caches handle millions of requests per second. The challenge is to implement cache operations that are:
- **Thread-safe** without explicit locking
- **Reliable** with predictable behavior
- **Concurrent** to handle multiple operations efficiently

This project simulates an in-memory cache system (similar to Redis/Memcached) using pure functional programming principles. By representing all state transformations as pure functions, we achieve **implicit thread-safety** and **testability** without mutable variables or locks.

### Industrial Motivation
- **Backend Systems**: Web servers use caches to store session data, API responses, and database query results
- **Microservices**: Service meshes employ distributed caches for configuration and service discovery
- **Data Processing**: ETL pipelines cache intermediate results to avoid recomputation
- **CDNs**: Content delivery networks cache static assets with sophisticated eviction policies

---

## Features
✓ **LRU (Least Recently Used)** and **FIFO (First In First Out)** eviction policies  
✓ **Pure functional** implementation - all operations are immutable  
✓ **Type-safe** operations using Algebraic Data Types  
✓ **Thread-safe by design** - no mutable state or locks needed  
✓ **Cache statistics** tracking (hits, misses, hit rate)  
✓ **Interactive mode** for manual testing  
✓ **Automated demo** showing common operations  

---

## Instructions to Run the Program

### Prerequisites
- GHC (Glasgow Haskell Compiler) 8.10 or higher
- GHCi for interactive execution

### Quick Start

#### Method 1: Using GHCi (Recommended)
```bash
ghci Main.hs
main
```

#### Method 2: Compile and Run
```bash
ghc -O2 Main.hs -o cache-simulator
./cache-simulator
```

#### Method 3: Using runghc
```bash
runghc Main.hs
```

---

## Sample Input/Output

### Automated Demo Mode
```
Choice (default 1): 1
Enter cache capacity (default 5): 5
Select eviction policy (1=LRU, 2=FIFO, default=LRU): 1

--- Running Automated Demo ---

Loading test data...
Loaded 5 test entries.

--- Cache Contents ---
user:1001 -> John Doe (accessed: 1)
user:1002 -> Jane Smith (accessed: 2)
...

Performing lookups...
✓ HIT: John Doe
✗ MISS: Key not found

Inserting with eviction (cache full)...
⚠ Evicted key: user:1002
```

### Interactive Mode
```
Choice (default 1): 2

> put session:123 user_data_xyz
✓ Cache updated successfully

> get session:123
✓ HIT: user_data_xyz

> stats
--- Cache Statistics ---
Total Hits:     1
Total Misses:   0
Hit Rate:       100.00%
Current Size:   1/5
```

---

## Functional Programming Concepts Used

### 1. Pure Functions
All core operations are pure - no side effects, same input always gives same output.

### 2. Immutability
State is never modified; new states are created for each operation.

### 3. Algebraic Data Types (ADTs)
Type-safe representations: `EvictionPolicy`, `CacheResult`, `CacheOperation`.

### 4. Recursion
Interactive loop uses recursion instead of loops.

### 5. Higher-Order Functions
Functions like `map`, `foldl` transform data elegantly.

### 6. Type Safety
Strong typing prevents many runtime errors at compile time.

### 7. Pattern Matching
Elegant case handling for different operation types.

### 8. Separation of Pure/Impure Code
Processing.hs (pure) vs IOHandler.hs (IO).

---

## Project Structure

```
cache-simulator/
├── Main.hs           - Entry point
├── DataTypes.hs      - Type definitions
├── Processing.hs     - Pure cache logic
├── IOHandler.hs      - I/O operations
├── Utils.hs          - Utility functions
└── README.md         - Documentation
```

---

## Testing

Test individual functions in GHCi:
```haskell
ghci Processing.hs
let cache = createCache 3 LRU
let (result, cache1) = insertCache "key1" "value1" cache
```

---

## Why Functional Programming for Caching?

1. **Thread-Safety by Default** - Pure functions eliminate race conditions
2. **Testability** - No setup/teardown needed
3. **Reliability** - Predictable behavior
4. **Composability** - Small functions combine easily
5. **Maintainability** - Clear data flow

---
