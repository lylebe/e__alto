#!/bin/bash
shopt -s nullglob
FILTERS=mapfilter*.json
for f in $FILTERS 
do
  echo "Testing file $f"
  ./getFilteredMap.sh $f
  sleep 1s
  printf "\n\n\n\n\n\n"
done
