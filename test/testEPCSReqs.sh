#!/bin/bash
shopt -s nullglob
FILTERS=epcs*.json
for f in $FILTERS 
do
  echo "Testing file $f"
  ./getEPCosts.sh $f
  sleep 1s
  printf "\n\n\n\n\n\n"
done
