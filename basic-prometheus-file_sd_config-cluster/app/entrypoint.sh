#!/bin/sh

# Run the Mojolicious app in the background
PORT=${1:-80}

PIDFILE='/run/app.pid'; 
perl myapp.pl daemon -l "http://*:$PORT" &
echo "started perl myapp.pl daemon -l \"http://*:$PORT\" &"

PID=$(ps ax|grep [p]erl|tail -1| awk '{print $1}')
if [ ! -z $PID ] ; 
then 
  echo $PID>$PIDFILE 
fi
while [ ! -f $PIDFILE ] 
do 
echo 'wait for app pid'
sleep 1 
done 
echo 'app is running with ID '$(cat $PIDFILE)
# NOTE: should not exit 
while true 
do 
  pgrep -P $(cat $PIDFILE) > /dev/null 
  if [ $? != 0 ] 
  then 
    echo 'app is gone'
    ps
    # exit 0
  fi 
  sleep 10
done



