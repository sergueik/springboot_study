#!/usr/bin/bash

text=$1
  reverse=""
i=0
if  [ -z $text  ] ;then
echo 'usage:'
exit 1 
fi
while [[ $i -lt ${#text} ]] ;  
  do
    reverse=`echo ${text:i:1}$reverse`
((i++ ))
  done
  
  if [[ $text == $reverse ]]
  then
    echo "Text is a Palindrome"
  else
    echo "Text is NOT a Palindrome"
  fi
