#!/bin/sh

#Copyright (c) 2020 Serguei Kouzmine
#
# beam server waiter script prototype

# Name of the service provider node or the node equipped with the beam server
# configured to start after completion of a lengthy setup process
if [ -z "${SERVICE_HOST}" ] ; then
  SERVICE_HOST='127.0.0.1' 
fi

# Public TCP port of the service or custom TCP port of the beam server
if [ -z "${SERVICE_PORT}" ] ; then
  SERVICE_PORT='10000'
fi
DELAY='60'

while true
do
  echo "Waiting on the ${SERVICE_HOST} ${SERVICE_PORT}"
  nc -z $SERVICE_HOST $SERVICE_PORT
  if [ $? -eq 0 ]
  then
    echo 'Got Response'
    # Optionally shut down the "beam server" in the service host
    echo QUIT | telnet -r $SERVICE_HOST $SERVICE_PORT
    break
  fi
  echo "Waiting ${DELAY} sec"
  sleep $DELAY
done

# launch the dependency app -
# the original ENTRYPOINT
# java -jar $APP

