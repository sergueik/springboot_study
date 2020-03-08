#!/bin/sh
# Copyright (c) 2020 Serguei Kouzmine
#
# beam server checker Docker ENTRYPOINT wrapper example

if [ -z "${SERVICE_HOST}" ] ; then
  SERVICE_HOST='mysql-server'
fi
if [ -z "${SERVICE_PORT}" ] ; then
  SERVICE_PORT='3306'
fi
DELAY='60'
APP='app.jar'

if $DEBUG_DELAYED_START; then
  LOG='/tmp/debug.log'
fi
while true
do
  if $DEBUG_DELAYED_START; then
    echo "Waiting on the ${SERVICE_HOST} ${SERVICE_PORT}" | tee -a $LOG
  else
    echo "Waiting on the ${SERVICE_HOST} ${SERVICE_PORT}"
  fi
  nc -z $SERVICE_HOST $SERVICE_PORT
  if [ $? -eq 0 ]
  then
    if $DEBUG_DELAYED_START; then
      echo 'Got Response' | tee -a $LOG
    else
      echo 'Got Response'
    fi
    break
  fi
  if $DEBUG_DELAYED_START; then
    echo "Waiting ${DELAY} sec" | tee -a $LOG
  else
    echo "Waiting ${DELAY} sec"
  fi
  sleep $DELAY
done
java -jar $APP
