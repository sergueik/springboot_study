#!/bin/sh

MAX_RESTARTS=5
COUNT_FILE=/tmp/restarts

count=$(cat $COUNT_FILE 2>/dev/null || echo 0)

if [ "$count" -ge "$MAX_RESTARTS" ]; then
  echo "Max restarts reached, exiting permanently"
  exit 1
fi

count=$((count+1))
echo $count > $COUNT_FILE

exec your_app
