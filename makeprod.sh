#!/bin/sh

ghc -O2 -o dnp.warp --make -threaded -DPRODUCTION warp.hs

