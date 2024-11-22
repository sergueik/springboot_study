#!/bin/bash

for (( i=1; i<=5; i++ )); do
if ((i%2 == 0 )) ; then continue ; fi
  echo "Iteration $i"
if [ $(expr $i % 2 == 1) ] ; then
 echo 'check'
fi 
done
  