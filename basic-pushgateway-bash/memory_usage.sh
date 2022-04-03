#!/bin/bash
# based on: https://github.com/SCHKN/pushgateway-scripts/blob/master/memory_usage
# Parsing ps aux lines, formats them and sends them to pushgateway.
# identical to cpu_usage.sh - the only difference is in
# the field processing awk script expression
# USER PID %CPU %MEM VSZ RSS TTY STAT START TIME COMMAND
# 1    2   3    4    5   6   7   8    9     10   11
while sleep 1
do
# using default ps fields - this will work on RHEL or Debian but not Alpine
COMMAND='ps aux'
COMMAND_OUTPUT=$($COMMAND)
VAR=''
while read -r COMMAND_OUTPUT
do
#if grep -q '%MEM' ; then
#  continue
#fi
  VAR=$VAR$(/usr/bin/awk '{print "memory_usage{process=\""$11"\", pid=\""$2"\"}", $4z}');
done <<< "$COMMAND_OUTPUT"
# NOTE: preserving newlines by enclosing in the quotes
echo "$VAR"
if /bin/false ; then
curl -X POST -H  "Content-Type: text/plain" --data "$VAR
" http://localhost:9091/metrics/job/top/instance/machine
fi
done

