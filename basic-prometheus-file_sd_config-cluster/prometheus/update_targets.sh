#!/bin/ash

TS=$(date +%s)
TARGET_FILE_PATH='/etc/prometheus'
TMPFILE="${TARGET_FILE_PATH}/dynamic_targets.json.tmp"
cat <<EOF>$TMPFILE
[
  {
    "targets": ["http%3A%2F%2Fapp%3A80%2Fdata%3Fts%3D$TS"],
    "labels": {
      "module": "stub"
    }
  }
]
EOF
mv $TMPFILE ${TARGET_FILE_PATH}/dynamic_targets.json
