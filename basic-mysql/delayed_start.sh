#!/bin/sh

DB_HOST='mysql-server'
DB_PORT='3306'
APP='app.jar'
while true
do
nc -z $DB_HOST $DB_PORT
if [ $? -eq 0 ]
then
break
fi
echo "Waiting on the ${DB_HOST} ${DB_PORT}"
sleep 10
done

java -jar $APP
