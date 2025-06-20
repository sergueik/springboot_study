#!/usr/bin/bash

declare -A my_hash

my_hash["key1"]="value1"
my_hash["key2"]="value2"
unset my_hash["key2"]	
	declare -A my_hash=(
    ["username"]="admin"
    ["password"]="secret"
    ["host"]="localhost"
)

my_hash["key1"]="value1"

for key  in "${!my_hash[@]}" ; do
echo $key
echo "${my_hash["$key"]}"
done
# NO  dollar  symbol
key1='password' 
if [[ -v ${my_hash[$key1]} ]] ;  then
echo "$key1 is defined"
else
echo "$key1 is not defined"
fi
if [[ -v my_hash[$key1] ]] ;  then
	echo "$key1 is defined"
else
	echo "$key1 is not defined"
fi
