#!/bin/sh

DB_HOST='mysql-server'
DB_PORT='3306'
DELAY='60'
APP='app.jar'

while true
do
  echo "Waiting on the ${DB_HOST} ${DB_PORT}"
  nc -z $DB_HOST $DB_PORT
  if [ $? -eq 0 ]
  then
    echo 'Got Response'
    break
  fi
  echo "Waiting ${DELAY} sec"
  sleep $DELAY
done

java -jar $APP

