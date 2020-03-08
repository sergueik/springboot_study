#!/bin/sh

APP='app.jar'

if [ -z "${SERVICE_HOST}" ] ; then
  SERVICE_HOST='mysql-server'
fi
if [ -z "${SERVICE_PORT}" ] ; then
  SERVICE_PORT='3306'
fi
DELAY='60'

if $DEBUG; then
  DEBUG_LOG='/tmp/debug.log'
fi
while true
do
  if $DEBUG; then
    echo "Waiting on the ${SERVICE_HOST} ${SERVICE_PORT}" | tee -a $DEBUG_LOG
  else
    echo "Waiting on the ${SERVICE_HOST} ${SERVICE_PORT}"
  fi
  nc -z $SERVICE_HOST $SERVICE_PORT
  if [ $? -eq 0 ]
  then
    if $DEBUG; then
      echo 'Got Response' | tee -a $DEBUG_LOG
    else
      echo 'Got Response'
    fi
    break
  fi
  if $DEBUG; then
    echo "Waiting ${DELAY} sec" | tee -a $DEBUG_LOG
  else
    echo "Waiting ${DELAY} sec"
  fi
  sleep $DELAY
done
java -jar $APP
