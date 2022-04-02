#!/bin/bash
# based on: https://github.com/SCHKN/pushgateway-scripts/blob/master/cpu_usage
# Parsing ps aux lines, formats them and sends them to pushgateway.
INSTANCE=$(/bin/hostname -f)
JOB=${0##*/} # remove the path
## Pushgateway VARs
HOST=${1:-pushgateway}
PORT=9091
# remove the extension
JOB=${JOB%.*}
# remove the domain suffix
INTANCE=${INSTANCE%.*}
PATH="/metrics/job/${JOB}/instance/${INSTANCE}"
URL="http://${HOST}:${PORT}${PATH}"
# e.g. http://localhost:9091/metrics/job/top/instance/machine
while /bin/sleep 1
do
COMMAND_OUTPUT=$(/bin/ps aux| /usr/bin/head -5)
VAR=''
while read -r COMMAND_OUTPUT
# below is bash syntax, not recognized by older shells:
# syntax error: unexpected redirection
# Syntax error: redirection unexpected
do
   VAR=$VAR$(/usr/bin/awk '{print "cpu_usage{process=\""$11"\", pid=\""$2"\"}", $3z}');
done <<< "$COMMAND_OUTPUT"
echo $VAR
if /bin/true ; then
/usr/bin/curl -X POST -H 'Content-Type: text/plain' --data "$VAR
" $URL
fi
done

