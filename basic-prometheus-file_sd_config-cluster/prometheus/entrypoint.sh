#!/bin/sh

# Optional: prometheus itself
prometheus --config.file=/prometheus.yml --web.listen-address=:9090 &


# Start mock cron loop — every 30s
while true; do
  echo "[$(date)] Updating timestamp..."
  ./update_targets.sh
  sleep 30
done



