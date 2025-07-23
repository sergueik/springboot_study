#!/bin/ash

TS=$(date +%s)
TARGET_FILE_PATH='/etc/prometheus'
TMPFILE="${TARGET_FILE_PATH}/dynamic_targets.json.tmp"
cat <<EOF>$TMPFILE
[
  {
    "targets": ["exporter:7979"],
    "labels": {
      "job": "myapp",
      "timestamp": "$TS",
      "target": "http://app:80/data?ts=$TS",
      "module": "stub"
    }
  }
]


EOF
mv $TMPFILE ${TARGET_FILE_PATH}/dynamic_targets.json
