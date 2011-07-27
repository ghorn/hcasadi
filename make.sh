#!/bin/sh

ghc -Wall -O3 -o Hom Hom.hs -o spring spring.hs -fforce-recomp
