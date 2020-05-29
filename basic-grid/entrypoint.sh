#!/bin/sh

# The docker-compose.yml approach is enforcing environment usage
source stop_trap.sh
ls $SELENIUM_JAR
java -jar $SELENIUM_JAR \
  -role hub \
  -port $SELENIUM_HUB_PORT \
  &

JAVA_PID=$!
echo "JAVA_PID=${JAVA_PID}"
wait $JAVA_PID
sleep 1000
