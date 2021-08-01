#!/bin/bash
#This script automates running the AFL fuzz script.
#Takes in the desired output folder and number of minutes to fuzz.

if ! [ $# -eq 2 ] ; then
  echo "Incorrect number of arguments!. Must pass in arguments: OUTPUT_FOLDER MINUTES" >&2
  exit 1
fi

OUT=$1
minutes=$2

re='^[0-9]+$'
if ! [[ $minutes =~ $re ]] ; then
   echo "MINUTES argument must be a number" >&2
   exit 1
fi

((seconds=$minutes*60))
#Add 5 seconds to account for startup time to fuzzing.
((shifted=$seconds+5))
time_string=${shifted}s

#Calls alf to fuzz for the specified period of time
timeout $time_string ~/AFL/afl-fuzz -i seeds -o ${OUT} -f input -- ./fuzzing/afl-proxy a2j j2a log

exit 0
