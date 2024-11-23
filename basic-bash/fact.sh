#!/bin/bash

function fact() {
  local num1=$1
  local result=1
  # local num2=$((num1 - 1))
  if [ $num1 -le 0 ] ; then
	  exit -1  
  elif [ $num1 -eq 1 ] ; then
	  result=1
  else
  local num2=$((num1 - 1))
	  # echo 1>&2 $num2
	  result=$(fact $num2)
	  result=$((result * num1))
	  fi
	  echo 1>&2  "in function: ${result}"
  echo $result
}
NUM=${1:-5}
result=$(fact $NUM)
echo "The result: $result"
