#! /bin/sh
./wait-for-it.sh jm-postgres:5432 -t 15
java -Djava.security.egd=file:/dev/./urandom -jar app.jar
