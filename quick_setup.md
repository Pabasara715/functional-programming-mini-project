# Setup and Installation Guide

## Quick Start (5 minutes)

### Step 1: Install GHC

**Ubuntu/Debian:**
```bash
sudo apt-get update
sudo apt-get install ghc
```

**macOS (Homebrew):**
```bash
brew install ghc
```

**Windows:**
Download from: https://www.haskell.org/ghcup/

### Step 2: Create Project Directory
```bash
mkdir cache-simulator
cd cache-simulator
```

### Step 3: Create All Files
Save each `.hs` file provided in the project:
- `DataTypes.hs`
- `Utils.hs`
- `Processing.hs`
- `IOHandler.hs`
- `Main.hs`

### Step 4: Install Required Package
```bash
# Install parallel package if not already installed
cabal update
cabal install --lib parallel
```

### Step 5: Run the Program

**Method A: Using GHCi (Interactive)**
```bash
ghci Main.hs
```
Then in GHCi:
```haskell
*Main> main
```

**Method B: Compile and Run**
```bash
ghc -O2 -threaded Main.hs -o cache-simulator
./cache-simulator
```

**Method C: Direct Execution**
```bash
runghc Main.hs
```

---

## Detailed Setup Instructions

### For Windows Users

1. **Install GHCup** (recommended):
   - Visit: https://www.haskell.org/ghcup/
   - Run the installer
   - Follow the prompts to install GHC

2. **Alternative: Haskell Platform**:
   - Download from: https://www.haskell.org/platform/
   - Install the complete platform

3. **Verify Installation**:
   ```powershell
   ghc --version
   ghci --version
   ```

4. **Run the Project**:
   ```powershell
   cd cache-simulator
   ghci Main.hs
   *Main> main
   ```

### For macOS Users

1. **Install Homebrew** (if not installed):
   ```bash
   /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
   ```

2. **Install GHC**:
   ```bash
   brew install ghc cabal-install
   ```

3. **Verify Installation**:
   ```bash
   ghc --version
   cabal --version
   ```

4. **Run the Project**:
   ```bash
   cd cache-simulator
   ghci Main.hs
   *Main> main
   ```

### For Linux Users

1. **Ubuntu/Debian**:
   ```bash
   sudo apt-get update
   sudo apt-get install ghc cabal-install
   ```

2. **Fedora/RHEL**:
   ```bash
   sudo dnf install ghc cabal-install
   ```

3. **Arch Linux**:
   ```bash
   sudo pacman -S ghc cabal-install
   ```

4. **Verify Installation**:
   ```bash
   ghc --version
   cabal --version
   ```

---

## Dependency Management

### Required Packages

The project uses these standard libraries:
- `base` (included with GHC)
- `containers` (included with GHC)
- `parallel` (needs installation)

### Installing Dependencies

**Using Cabal:**
```bash
cabal update
cabal install --lib parallel
```

**Using Stack (alternative):**
```bash
stack setup
stack build
stack exec cache-simulator
```

---

## Compilation Options

### Basic Compilation
```bash
ghc Main.hs -o cache-simulator
```

### Optimized Compilation
```bash
ghc -O2 Main.hs -o cache-simulator
```

### With Threading Support
```bash
ghc -O2 -threaded Main.hs -o cache-simulator
```

### Run with Multiple Cores
```bash
./cache-simulator +RTS -N4  # Use 4 cores
```

---

## Troubleshooting

### Problem: "Module not found"

**Solution:**
```bash
# Make sure all files are in the same directory
ls -la
# Should show: Main.hs, DataTypes.hs, Utils.hs, Processing.hs, IOHandler.hs

# Try loading with explicit path
ghci -i. Main.hs
```

### Problem: "Package 'parallel' not found"

**Solution:**
```bash
cabal update
cabal install --lib parallel

# Alternative: use GHC without parallel features
# Comment out parallelLookups function in Processing.hs
```

### Problem: "Parse error" or compilation fails

**Solution:**
- Check that all files are saved with UTF-8 encoding
- Ensure no Windows line endings (CRLF) if on Linux/Mac
- Verify all files are copied completely

### Problem: GHC version too old

**Solution:**
```bash
# Check version
ghc --version

# Need GHC 8.10 or higher
# Upgrade using ghcup:
ghcup install ghc 9.2.5
ghcup set ghc 9.2.5
```

---

## IDE Setup (Optional)

### VS Code
1. Install "Haskell" extension
2. Install "Haskell Syntax Highlighting"
3. Open project folder
4. Extension will auto-detect GHC

### IntelliJ IDEA
1. Install "IntelliJ-Haskell" plugin
2. Import project
3. Configure GHC path in settings

### Emacs
1. Install `haskell-mode`
2. Configure with `ghci`

---

## Running Tests

### Interactive Testing
```bash
ghci Processing.hs

# Test functions directly
let cache = createCache 5 LRU
let (r, c) = insertCache "test" "value" cache
print r
```

### Automated Tests (optional)
Create `Tests.hs`:
```haskell
module Tests where

import DataTypes
import Utils
import Processing

testInsert = 
    let cache = createCache 5 LRU
        (result, newCache) = insertCache "key1" "value1" cache
    in result == Updated

testLookup = 
    let cache = createCache 5 LRU
        (_, cache1) = insertCache "key1" "value1" cache
        (result, _) = lookupCache "key1" cache1
    in result == Hit "value1"

runTests = do
    putStrLn $ "Test Insert: " ++ show testInsert
    putStrLn $ "Test Lookup: " ++ show testLookup
```

Run tests:
```bash
ghci Tests.hs
*Tests> runTests
```

---

## Performance Tips

1. **Compile with optimizations**:
   ```bash
   ghc -O2 -threaded Main.hs
   ```

2. **Use multiple cores**:
   ```bash
   ./cache-simulator +RTS -N4 -RTS
   ```

3. **Profile performance**:
   ```bash
   ghc -prof -fprof-auto Main.hs
   ./cache-simulator +RTS -p
   cat Main.prof  # View profiling results
   ```

---

## Project Structure Verification

Your directory should look like:
```
cache-simulator/
├── Main.hs
├── DataTypes.hs
├── Processing.hs
├── IOHandler.hs
├── Utils.hs
├── README.md
└── SETUP_GUIDE.md (this file)
```

Verify all files are present:
```bash
ls -1 *.hs
# Should output:
# DataTypes.hs
# IOHandler.hs
# Main.hs
# Processing.hs
# Utils.hs
```

---

## Getting Help

If you encounter issues:

1. **Check GHC version**: `ghc --version` (need 8.10+)
2. **Verify file integrity**: Ensure all `.hs` files are complete
3. **Review error messages**: GHC provides helpful type errors
4. **Test modules individually**: Load each `.hs` file in GHCi

Common commands for debugging:
```bash
# Load specific module
ghci DataTypes.hs

# Check types
:t lookupCache

# Show info about a function
:info CacheState

# Reload after changes
:reload
```

---

## Next Steps

After successful setup:

1. Run the automated demo: Choose option 1
2. Try interactive mode: Choose option 2
3. Experiment with different cache sizes
4. Test both LRU and FIFO policies
5. Review the code to understand FP concepts

---

## Support

For course-related questions, contact your instructor or TA.

For technical Haskell questions:
- Official Haskell documentation: https://www.haskell.org/documentation/
- GHC User Guide: https://downloads.haskell.org/ghc/latest/docs/html/users_guide/
