#!/bin/sh
if [ -z "${SERVICE_HOST}" ] ; then
  SERVICE_HOST='mongo-service'
fi
if [ -z "${SERVICE_PORT}" ] ; then
  SERVICE_PORT='27017'
fi
DELAY='60'
APP='app.jar'

while true
do
  echo "Waiting on the ${SERVICE_HOST} ${SERVICE_PORT}"
  nc -z $SERVICE_HOST $SERVICE_PORT
  if [ $? -eq 0 ]
  then
    echo 'Got Response'
    break
  fi
  echo "Waiting ${DELAY} sec"
  sleep $DELAY
done

java -jar $APP

