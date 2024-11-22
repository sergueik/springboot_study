#!/bin/bash
#note process substitution does not work the same in the interctive shell  prompt

x=<(ls -l)
echo $x

while

IFS='' 
	read line; do
  echo "$line"
done < <(ls -l)
while

IFS=',' 
	read col1 col2 col3; do
  printf "col1:%s col2:%s col3:%s\n" "$col1" "$col2" "$col3"
done < <(echo -e "a1,b1,c1\na2,b2,c2")
