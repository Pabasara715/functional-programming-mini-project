#!/bin/bash
# Simple test script

echo "Testing compilation..."
ghc Main.hs -o cache-simulator

if [ $? -eq 0 ]; then
    echo "✓ Compilation successful!"
    echo ""
    echo "Running cache simulator..."
    echo "1" | ./cache-simulator
    echo ""
    echo "✓ Test complete!"
else
    echo "✗ Compilation failed!"
    exit 1
fi
