#!/bin/bash


fib(){
local num=$1
local result=0
local r1
local r2
if [[ $num -lt 0  || $num -gt 10 ]] ; then
exit 1
elif [ $num -eq 0 ]; then
  result=0
elif [ $num -eq 1 ] ;then
  result=1
else
  num=$(( num-1 ))
  r1=$(fib $num)
  ((--num))
  r2=$(fib $num)
  result=$((r1  + r2))
fi
echo $result
}
NUM=${1:-5}
RESULT=$(fib $NUM)

echo $RESULT
