#!/bin/bash
# origin: https://github.com/SCHKN/pushgateway-scripts/blob/master/memory_usage
# Parsing ps aux lines, formats them and sends them to pushgateway.
while sleep 1 
do
z=$(ps aux)
var=""
while read -r z 
do
   var=$var$(awk '{print "memory_usage{process=\""$11"\", pid=\""$2"\"}", $4z}');
done <<< "$z"
echo "$var"
curl -X POST -H  "Content-Type: text/plain" --data "$var
" http://localhost:9091/metrics/job/top/instance/machine
done
