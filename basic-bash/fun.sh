#!/bin/bash

function add_numbers() {
  local num1=$1
  local num2=$2
  local sum=$((num1 + num2))
  echo $sum 
}

result=$(add_numbers 5 7)
echo "The sum is: $result"
