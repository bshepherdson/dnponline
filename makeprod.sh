#!/bin/sh

rm *.hi *.o Handler/*.{hi,o}
ghc -O2 -o dnp.warp --make -threaded -DPRODUCTION warp.hs

