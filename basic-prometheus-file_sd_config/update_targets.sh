#!/bin/ash

TS=$(date +%s)
TARGET_FILE_PATH='/etc/prometheus'
cat <<EOF> ${TARGET_FILE_PATH}/dynamic_targets.json
[
  {
    "targets": [ "localhost:7979" ],
    "labels": {
      "job": "stub",
      "timestamp": "$TS"
    }
  }
]
EOF

