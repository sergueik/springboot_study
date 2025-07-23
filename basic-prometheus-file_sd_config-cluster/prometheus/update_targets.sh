#!/bin/ash

TS=$(date +%s)
TARGET_FILE_PATH='/etc/prometheus'
cat <<EOF> ${TARGET_FILE_PATH}/dynamic_targets.json
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
