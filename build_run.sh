#!/bin/sh
#ghc --make Main.hs compress.c && ./Main
cabal configure --enable-executable-profiling && cabal build && dist/build/bonedewd-server/bonedewd-server +RTS -H100M -N1 -s -p -RTS
