#!/bin/bash
shopt -s nullglob
FILTERS=costmap_filter*.json
for f in $FILTERS 
do
  echo "Testing file $f"
  ./getFilteredCostMap.sh $f
  sleep 1s
  printf "\n\n\n\n\n\n"
done
