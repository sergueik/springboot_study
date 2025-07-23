#!/bin/sh

/bin/prometheus --config.file=/prometheus.yml --web.enable-lifecycle --storage.tsdb.path=/prometheus &


# Start mock cron loop — every 30s
while true; do
  echo "[$(date)] Updating timestamp..."
  ./update_targets.sh
  sleep 30
done



