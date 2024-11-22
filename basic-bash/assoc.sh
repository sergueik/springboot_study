#!/usr/bin/bash

declare -A my_hash
my_hash["key1"]="value1"
my_hash["key2"]="value2"


for key  in "${!my_hash[@]}" ; do
echo $key
echo "${my_hash["$key"]}"
done
# NO  dollar  symboil
if [[ -v $my_hash["key1"] ]] ;  then
echo "$key is defined"
else
echo "$key is not defined"
fi
if [[ -v my_hash["key1"] ]] ;  then
	echo "$key is defined"
else
	echo "$key is not defined"
fi
