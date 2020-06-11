#!/bin/sh
# Copyright (c) 2020 Serguei Kouzmine
#
# beam server checker Docker ENTRYPOINT wrapper example

if [ -z "${SERVICE_HOST}" ] ; then
  echo 'Using default service host'
  SERVICE_HOST='mysql-server'
fi
if [ -z "${SERVICE_PORT}" ] ; then
  echo 'Using default service port'
  SERVICE_PORT='3306'
fi
DELAY='60'
APP='app.jar'
RETRY_CNT=0
MAX_RETRY=10
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
  RETRY_CNT=$(expr $RETRY_CNT + 1)
  if [[ $RETRY_CNT > $MAX_RETRY ]] ; then
    echo "Purging ${LOGDIR}..."
    break
  fi
done

/usr/bin/which java
if [ $? -eq 0 ]
then
  echo 'found java'
  java -jar $APP
fi
