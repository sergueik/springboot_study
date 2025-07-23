#!/bin/ash

TS=$(date +%s)
TARGET_FILE_PATH='/etc/prometheus'
cat <<EOF> ${TARGET_FILE_PATH}/dynamic_targets.json
[
  {
    "targets": ["http://localhost:80/data?ts=$TS"],
    "labels": {
    "job": "myapp"
    
  }
]
EOF

