# 5-Minute Video Demo Script
## Pure Concurrent Data Cache Simulator

---

## 🎬 SCRIPT OVERVIEW

**Total Time:** 5 minutes  
**Sections:**
1. Introduction (30 seconds)
2. Project Structure & Design (45 seconds)
3. Live Demo - Automated Mode (60 seconds)
4. Live Demo - Interactive Mode (90 seconds)
5. FP Concepts Explanation (60 seconds)
6. Conclusion (15 seconds)

---

## 📝 DETAILED SCRIPT WITH ACTIONS

### [0:00 - 0:30] SECTION 1: INTRODUCTION (30 seconds)

**WHAT TO SHOW:** Title slide or VS Code with README open

**WHAT TO SAY:**
> "Hello! Today we're presenting our Functional Programming mini project: A Pure, Concurrent Data Cache Simulator. This project simulates an in-memory cache system like Redis or Memcached, but built entirely with pure functional programming principles in Haskell. The key innovation is achieving thread-safety and reliability without using any mutable state or locks."

**ACTIONS:**
- Show project title on screen
- Show team member names
- Briefly show README.md

**SCREEN RECORDING:**
```bash
# Have VS Code open with README.md visible
# Camera can show your face in corner
```

---

### [0:30 - 1:15] SECTION 2: PROJECT STRUCTURE & DESIGN (45 seconds)

**WHAT TO SHOW:** VS Code file explorer + DataTypes.hs

**WHAT TO SAY:**
> "Our project follows a modular architecture with five Haskell modules. DataTypes defines our algebraic data types - notice we have EvictionPolicy with LRU and FIFO options, CacheResult representing different operation outcomes, and CacheState which is completely immutable. Utils provides pure helper functions, Processing contains our core cache operations - all pure functions - IOHandler isolates all side effects, and Main orchestrates the program flow. This separation makes our code testable and reliable."

**ACTIONS:**
1. Show file tree in VS Code (0:30-0:35)
```
cache-simulator/
├── Main.hs
├── DataTypes.hs
├── Processing.hs
├── IOHandler.hs
└── Utils.hs
```

2. Open DataTypes.hs and highlight (0:35-0:55):
```haskell
data EvictionPolicy = LRU | FIFO

data CacheResult 
    = Hit CacheValue 
    | Miss 
    | Updated 
    | Evicted CacheKey

data CacheState = CacheState
    { entries :: Map CacheKey CacheEntry
    , capacity :: Int
    , policy :: EvictionPolicy
    , hitCount :: Int
    , missCount :: Int
    }
```

3. Quick scroll through Processing.hs showing function signatures (0:55-1:15):
```haskell
lookupCache :: CacheKey -> CacheState -> (CacheResult, CacheState)
insertCache :: CacheKey -> CacheValue -> CacheState -> (CacheResult, CacheState)
```

**KEY POINTS TO MENTION:**
- ✓ Modular design
- ✓ ADTs for type safety
- ✓ Pure functions (no side effects)
- ✓ Immutable state

---

### [1:15 - 2:15] SECTION 3: AUTOMATED DEMO (60 seconds)

**WHAT TO SHOW:** Terminal running the program

**WHAT TO SAY:**
> "Let's see it in action. I'll start the program in GHCi and run the automated demo mode. We're creating a cache with capacity 5 using LRU eviction policy. The demo loads test data - notice we have user and product entries. Now it performs lookup operations - here's a cache hit for user 1001, and here's a miss for a non-existent key. Now watch this: the cache is full with 5 entries, so when we insert a new item, it automatically evicts the least recently used key - user 1002 gets removed. The statistics show our hit rate and cache utilization. All of this happens with pure, immutable operations."

**ACTIONS:**

1. Open terminal and start GHCi (1:15-1:20):
```bash
cd cache-simulator
ghci Main.hs
```

2. Type and execute (1:20-1:25):
```haskell
main
```

3. Select automated demo (1:25-1:30):
```
Choice (default 1): 1         [Press Enter]
Enter cache capacity: 5       [Press Enter]
Select eviction policy: 1     [Press Enter]
```

4. Narrate as output appears (1:30-2:00):
```
--- Running Automated Demo ---

Loading test data...
Loaded 5 test entries.

--- Cache Contents ---
user:1001 -> John Doe (accessed: 1)
user:1002 -> Jane Smith (accessed: 2)
user:1003 -> Bob Johnson (accessed: 3)
product:201 -> Laptop (accessed: 4)
product:202 -> Mouse (accessed: 5)

Performing lookups...
✓ HIT: John Doe                    ← POINT THIS OUT
✗ MISS: Key not found              ← POINT THIS OUT

Inserting with eviction (cache full)...
⚠ Evicted key: user:1002           ← POINT THIS OUT

--- Cache Statistics ---
Total Hits:     1
Total Misses:   1
Hit Rate:       50.00%             ← POINT THIS OUT
Current Size:   5/5
```

5. Pause and point to screen (2:00-2:15):
- Circle or highlight the evicted key
- Circle the statistics

**KEY POINTS TO MENTION:**
- ✓ Cache hit and miss
- ✓ Automatic LRU eviction when full
- ✓ Statistics tracking
- ✓ Pure functional operations

---

### [2:15 - 3:45] SECTION 4: INTERACTIVE MODE (90 seconds)

**WHAT TO SHOW:** Terminal with interactive commands

**WHAT TO SAY:**
> "Now let me demonstrate the interactive mode with a smaller cache to better show the eviction behavior. I'm creating a cache with capacity 3. Let's add three items - session 1, 2, and 3. Now I'll lookup session 1 - cache hit with the correct value. Let's check statistics - 1 hit, 0 misses, 100% hit rate, and we're at full capacity. Now here's where it gets interesting: I'll insert a fourth item. Watch - session 2 gets evicted because it was the least recently used. Remember, we accessed session 1 just before, so it stayed in the cache - that's LRU policy in action, and all of this is implemented with pure functions. Let me verify the eviction worked - trying to get session 2 - it's a miss, as expected. Finally, I'll demonstrate clearing the cache and exiting."

**ACTIONS:**

1. Restart program and select interactive (2:15-2:25):
```bash
:quit
main
```
```
Choice: 2                      [Type 2, Enter]
Enter cache capacity: 3        [Type 3, Enter]
Select eviction policy: 1      [Type 1, Enter]
```

2. Insert three items (2:25-2:35):
```
> put session:101 user_data_alice
✓ Cache updated successfully

> put session:102 user_data_bob
✓ Cache updated successfully

> put session:103 user_data_charlie
✓ Cache updated successfully
```

3. Lookup to show hit (2:35-2:40):
```
> get session:101
✓ HIT: user_data_alice           ← POINT OUT
```

4. Check statistics (2:40-2:50):
```
> stats

--- Cache Statistics ---
Total Hits:     1
Total Misses:   0
Hit Rate:       100.00%          ← POINT OUT
Current Size:   3/3              ← POINT OUT: FULL
------------------------
```

5. Insert fourth item to trigger eviction (2:50-3:05):
```
> put session:104 user_data_david
⚠ Evicted key: session:102       ← CIRCLE/HIGHLIGHT THIS
✓ Cache updated successfully
```

**PAUSE AND EXPLAIN:**
> "Notice session:102 was evicted, not session:101, because we accessed 101 recently with our get command. This is LRU - Least Recently Used - in action."

6. Verify eviction (3:05-3:15):
```
> get session:102
✗ MISS: Key not found            ← POINT OUT
```

7. Show it's working (3:15-3:20):
```
> get session:101
✓ HIT: user_data_alice           ← Still in cache!
```

8. Quick stats and clear (3:20-3:35):
```
> stats
--- Cache Statistics ---
Total Hits:     2
Total Misses:   1
Hit Rate:       66.67%
Current Size:   3/3

> clear
✓ Cache updated successfully
Cache cleared.
```

9. Show help and quit (3:35-3:45):
```
> help
[Shows all commands]

> quit
Thank you for using the cache simulator!
Goodbye!
```

**KEY POINTS TO MENTION:**
- ✓ User interaction
- ✓ LRU eviction policy working
- ✓ Statistics tracking hits/misses
- ✓ All operations are pure and thread-safe

---

### [3:45 - 4:45] SECTION 5: FP CONCEPTS EXPLANATION (60 seconds)

**WHAT TO SHOW:** Code in VS Code with key functions highlighted

**WHAT TO SAY:**
> "Now let's examine the functional programming concepts that make this work. First, purity - our lookup function takes a cache state and returns both a result and a new cache state, never modifying the original. Second, immutability - see how we create a new CacheState rather than mutating? This makes our code inherently thread-safe without locks. Third, algebraic data types - our CacheResult type uses pattern matching to handle all possible outcomes explicitly, and the compiler ensures we handle every case. Fourth, higher-order functions - we use map to transform data and foldl for batch operations. Fifth, recursion - our interactive loop uses recursion instead of while loops. And finally, the separation of pure and impure code - all business logic in Processing is pure, while IOHandler contains all side effects. These principles together create code that's reliable, testable, and naturally concurrent."

**ACTIONS:**

1. Show Processing.hs - Pure function (3:45-4:05):
```haskell
-- HIGHLIGHT THIS
lookupCache :: CacheKey -> CacheState -> (CacheResult, CacheState)
lookupCache key cache = 
    case Map.lookup key (entries cache) of
        Just entry -> 
            let updatedEntry = entry { lastAccessed = currentTime cache }
                updatedEntries = Map.insert key updatedEntry (entries cache)
                newCache = incrementTime $ cache 
                    { entries = updatedEntries
                    , hitCount = hitCount cache + 1 }
            in (Hit (value entry), newCache)  -- Returns NEW state
        Nothing -> (Miss, incrementTime $ cache { missCount = missCount cache + 1 })
```

**SAY:** "Notice: no mutation, returns new state"

2. Show immutability example (4:05-4:15):
```haskell
-- HIGHLIGHT THIS
insertCache key val cache = 
    let newEntry = CacheEntry val (currentTime cache)
        newCache = cache { entries = Map.insert key newEntry (entries cache) }
    in (Updated, newCache)  -- New cache, original unchanged
```

3. Show ADT pattern matching (4:15-4:25):
```haskell
-- HIGHLIGHT THIS
processOperation :: CacheOperation -> CacheState -> (CacheResult, CacheState)
processOperation op cache = case op of
    Lookup key -> lookupCache key cache
    Insert key val -> insertCache key val cache
    Evict key -> evictKey key cache
    Clear -> (Updated, clearCache cache)
```

**SAY:** "Compiler ensures we handle all cases"

4. Show higher-order function (4:25-4:30):
```haskell
-- HIGHLIGHT THIS
formatCache :: CacheState -> [String]
formatCache cache = map formatEntry (Map.toList (entries cache))
```

5. Show recursion (4:30-4:35):
```haskell
-- HIGHLIGHT THIS
interactiveLoop :: CacheState -> IO ()
interactiveLoop cache = do
    operation <- readCommand
    let (result, newCache) = processOperation operation cache
    displayResult result
    interactiveLoop newCache  -- Recursive call
```

6. Split screen showing separation (4:35-4:45):
- Left: Processing.hs (Pure)
- Right: IOHandler.hs (IO)

**SAY:** "Pure logic separate from side effects"

**KEY POINTS TO COVER:**
- ✓ Pure functions
- ✓ Immutability
- ✓ Algebraic Data Types (ADTs)
- ✓ Pattern matching
- ✓ Higher-order functions
- ✓ Recursion
- ✓ Separation of pure/impure code
- ✓ Type safety

---

### [4:45 - 5:00] SECTION 6: CONCLUSION (15 seconds)

**WHAT TO SHOW:** Slide with key benefits or back to README

**WHAT TO SAY:**
> "In conclusion, this project demonstrates how functional programming principles - purity, immutability, and strong typing - enable us to build reliable, thread-safe systems without the complexity of locks and mutable state. Our cache simulator showcases real-world applicability of FP in backend systems, making it safer, more testable, and naturally concurrent. Thank you for watching!"

**ACTIONS:**
- Show summary slide or README with key points
- Show team member names again
- End recording

**FINAL SLIDE TEXT:**
```
✓ Pure Functions → Predictability
✓ Immutability → Thread-Safety
✓ ADTs → Type Safety
✓ Recursion → FP Style
✓ Higher-Order Functions → Composability

Real-world applicability:
Backend caching, distributed systems, concurrent processing

Team: [Your Names]
```

---

## 🎯 PRE-RECORDING CHECKLIST

### Before You Start Recording:

- [ ] Terminal font size is LARGE (at least 16pt)
- [ ] VS Code zoom is increased (Ctrl + +)
- [ ] Test audio and microphone
- [ ] Close unnecessary applications
- [ ] Turn off notifications
- [ ] Have script printed or on second monitor
- [ ] Test run through once
- [ ] Prepare title slide
- [ ] Have project fully compiled and working

### Terminal Setup:
```bash
# Increase font size
# Use high contrast theme
# Clear terminal before recording: clear
```

### VS Code Setup:
```bash
# Zoom in: Ctrl + + (multiple times)
# Use high contrast theme
# Hide minimap: View → Show Minimap (uncheck)
# Hide activity bar: View → Appearance → Activity Bar (uncheck)
```

---

## 📋 TIMING BREAKDOWN (Practice This!)

| Time | Section | Key Action |
|------|---------|------------|
| 0:00-0:30 | Intro | Show title, explain project |
| 0:30-1:15 | Structure | Show files, ADTs, functions |
| 1:15-2:15 | Auto Demo | Run demo, show eviction |
| 2:15-3:45 | Interactive | Manual commands, LRU test |
| 3:45-4:45 | FP Concepts | Code walkthrough |
| 4:45-5:00 | Conclusion | Summary, thank you |

---

## 🎬 CAMERA TIPS

1. **Screen Recording:**
   - Use OBS Studio or QuickTime (Mac)
   - Record at 1920x1080 minimum
   - 30 FPS is fine

2. **Audio:**
   - Use external microphone if possible
   - Record in quiet room
   - Test audio levels first

3. **Video Editing (Optional):**
   - Add title card at beginning
   - Add transitions between sections
   - Add zoom effects on important code
   - Add text annotations for key points

---

## 💬 KEY PHRASES TO USE

- "Pure functional programming principles"
- "Thread-safe without locks"
- "Immutable data structures"
- "Type-safe operations"
- "Algebraic data types"
- "Pattern matching ensures correctness"
- "Separation of pure and impure code"
- "Real-world applicability"
- "Reliable and testable"
- "Naturally concurrent"

---

## 🔧 BACKUP PLAN (If Something Goes Wrong)

### If GHCi crashes:
```bash
# Have compiled version ready
./cache-simulator
```

### If demo doesn't work:
- Have screenshots ready
- Have pre-recorded backup demo
- Skip to code explanation

### If timing is off:
- **Running long?** Skip detailed stats explanation
- **Running short?** Add FIFO policy demo
- Practice with timer!

---

## 📝 SCRIPT VARIATIONS

### If You Have 7 Minutes:
Add these sections:
- Compare LRU vs FIFO (1 minute)
- Show parallel operations code (1 minute)

### If You Have 3 Minutes:
Remove these sections:
- Skip detailed code walkthrough
- Combine both demos into one
- Focus on: Demo → FP concepts → Done

---

## ✅ POST-RECORDING CHECKLIST

- [ ] Video is 5 minutes (±15 seconds)
- [ ] Audio is clear throughout
- [ ] All commands are visible
- [ ] Code is readable
- [ ] All FP concepts mentioned
- [ ] Team members credited
- [ ] Export at good quality (1080p)
- [ ] Test playback before submission

---


