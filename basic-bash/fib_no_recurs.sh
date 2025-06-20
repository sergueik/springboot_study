#!/bin/bash

# https://www.geeksforgeeks.org/fibonacci-series-bash/
echo "The Fibonacci series is : "
num=${1:-1}
# First Number of the
a=0

# Second Number of the
# Fibonacci Series
b=1
if [[ $num -lt 0  || $num -gt 10 ]] ; then
1>2 printf "invalid argunent: %s" $num
exit 1
elif [[ $num -eq 0 ]] ; then
    printf "%s %s\n" $num $a
else
for (( i=0; i<num; i++ ))
do
    #echo -n "$a "
    fn=$((a + b))
    printf "%s %s\n" $i $a
    a=$b
    b=$fn
done
fi
echo $fn
