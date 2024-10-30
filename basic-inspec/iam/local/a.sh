#!/bin/bash

# Check if the file exists
if [ ! -f "$1" ]; then
    echo "Error: File not found: $1"
    exit 1
fi
FILEPATH="$1"
# Read the contents of the file and set the environment variable
export MY_VARIABLE=$(cat $FILEPATH | tr -d '\n' | tr -d '\r') 

# Verify that the variable is set correctly
echo "MY_VARIABLE is set to: $MY_VARIABLE"
