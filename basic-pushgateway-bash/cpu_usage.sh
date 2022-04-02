#!/bin/sh
# based on: https://github.com/SCHKN/pushgateway-scripts/blob/master/cpu_usage
# Parsing ps aux lines, formats them and sends them to pushgateway.
INSTANCE=$(/bin/hostname -f)
JOB=${0##*/} # remove the path
## Pushgateway vars
HOST=${1:-pushgateway}
PORT=9091
# remove the extension
JOB=${JOB%.*}
# remove the domain suffix
INTANCE=${INSTANCE%.*}
PATH="/metrics/job/${JOB}/instance/${INSTANCE}"
URL="http://${HOST}:${PORT}${PATH}"
# e.g. http://localhost:9091/metrics/job/top/instance/machine
INTERVAL=1
LINES=5
TMPFILE="/tmp/data.$$"

1>&2 echo "TMPFILE=${TMPFILE}"
while /bin/sleep $INTERVAL
do
# NOTE: space-sensitive ?
# NO %CPU on alpine
1>$TMPFILE 2>/dev/null /bin/ps aux -o pid,comm,vsz | /usr/bin/head -$LINES
# NOTE the head has no effect with output redirection this way

1>&2 /usr/bin/wc $TMPFILE
VAR=$(
/bin/cat $TMPFILE |/usr/bin/head -$LINES| while read LINE
do
if echo $LINE |/bin/grep -q 'VSZ' ; then
  echo >/dev/null
else
ROW=$(echo $LINE |/usr/bin/awk '{print "cpu_usage{process=\""$2"\", pid=\""$1"\"}", $3z}')
echo $ROW'NL'
fi
done
)
# hack around losing the VAR after exising the loop
# may lead toa problem with miltiple entries on the same line

# https://stackoverflow.com/questions/16414410/delete-empty-lines-using-sed
# NOTE: 'chomp' does not work with /^\\s*$/d
echo '---'
echo $VAR|/bin/sed 's|NL|\n|g' | /bin/sed '/^[[:space:]]*$/d'
echo '---'
if /bin/true ; then
/usr/bin/curl -X POST -H 'Content-Type: text/plain' --data "$(echo $VAR|/bin/sed 's|NL|\n|g'| /bin/sed '/^[[:space:]]*$/d')
" $URL
fi
done


