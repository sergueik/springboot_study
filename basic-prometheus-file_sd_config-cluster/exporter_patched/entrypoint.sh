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

# Start JSON exporter
json_exporter --config.file=/json_exporter_config.yml --web.listen-address=:7979 &

# Optional: prometheus itself
# prometheus --config.file=/prometheus.yml --web.listen-address=:9090 &


# Start mock cron loop — every 30s
while true; do
  echo "[$(date)] Updating timestamp..."
  ./update_targets.sh
  sleep 30
done



