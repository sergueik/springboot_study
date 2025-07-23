#!/bin/sh

OUTFILE="/etc/prometheus/dynamic_targets.json"
TMPFILE="${OUTFILE}.tmp"

while true; do
  TS=$(date +%s)
  cat <<EOF > "$TMPFILE"
[
  {
    "targets": ["app:80"],
    "labels": {
      "module": "stub",
      "target": "http://app:80/data?ts=${TS}"
    }
  }
]
EOF
  mv "$TMPFILE" "$OUTFILE"
  sleep 15
done

