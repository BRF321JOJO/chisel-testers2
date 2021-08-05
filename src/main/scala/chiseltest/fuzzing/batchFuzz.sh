#!/bin/bash

for i in {1..4}
do
  echo "Starting fuzzing run: ${i}"
  src/main/scala/chiseltest/fuzzing/fuzz.sh results/TLI2C.rfuzz.TLULseed.LongSeed.MTC.DNC.255/${i}.out 30 rfuzz
done
