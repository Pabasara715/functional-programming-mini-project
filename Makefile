# Makefile for Cache Simulator

.PHONY: all build run clean test

all: build

build:
	ghc -O2 Main.hs -o cache-simulator

run:
	./cache-simulator

interactive:
	ghci Main.hs

clean:
	rm -f *.hi *.o cache-simulator Main

test:
	ghci Processing.hs
