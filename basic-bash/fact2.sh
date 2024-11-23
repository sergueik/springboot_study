#!/bin/bash

function fact2() {
  local num=$1
  local result=1

  if [ $num -le 0 ] ; then
    exit -1  
  else
  for ((i = 1 ; i <= num ; i++)) ; do
    # echo 1>&2 $i
    1>&2 printf "mul %s\n" "$i"
  result=$((result * i))
  done
  fi
  1>&2  printf "in function: %s\n" $result
  echo $result
}
NUM=${1:-5}
result=$(fact2 $NUM)
echo "The result: $result"
