#!/bin/bash
shopt -s nullglob
FILTERS=eps*.json
for f in $FILTERS 
do
  echo "Testing file $f"
  ./getEndpoints.sh $f
  sleep 1s
  printf "\n\n\n\n\n\n"
done
