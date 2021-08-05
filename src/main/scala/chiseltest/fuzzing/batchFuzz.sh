#!/bin/bash

for i in {1..3}
do
  echo "Starting fuzzing run: ${i}"
  src/main/scala/chiseltest/fuzzing/fuzz.sh results/TLI2C.TLUL.TLULseed.ShortSeed.MTC.DNC.255/${i}.out 20 tlul
done
