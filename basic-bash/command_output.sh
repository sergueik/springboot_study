#!/bin/bash

command="ls -l"  # Replace with your desired command

while read line; do
  echo "Processing: $line"
  # Do something with $line
done < <(eval "$command")
