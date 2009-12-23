#!/bin/sh
#ghc --make Main.hs compress.c && ./Main
cabal configure && cabal build && dist/build/bonedewd-server/bonedewd-server +RTS -N2 -s -RTS
