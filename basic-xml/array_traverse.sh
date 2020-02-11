#!/bin/sh
# origin: https://unix.stackexchange.com/questions/137566/arrays-in-unix-bourne-shell

array_traverse()
{
    for i in $(seq 1 $2)
    do
    current_value=$1$i
    echo $(eval echo \$$current_value)
    done
    return 1
}

ARRAY_1=one
ARRAY_2=two
ARRAY_3=333
array_traverse ARRAY_ 3
