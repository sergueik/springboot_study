#!/bin/bash

if [ -z "${DEBUG}" ]; then
  DEBUG=false
  1>&2 echo 'Setting DEBUG to ' $DEBUG
else
  DEBUG=true
fi
BASE_URL='http://localhost:9001'

RANGE=${1}
INTERVAL=${2:-6}
# NOTE: pass arbitrary on-empty arg to override 'recent'
echo "RANGE=${RANGE}"
if [ "${RANGE}" == 'recent' ]; then
  # when RRD feed is live the health check needs to calculate the recent date times for query
  # and be consistent with the Grafana datetime format
  DATE_FORMAT='%Y-%m-%dT%H:%M:%SZ'
  END_DATETIME=$(date -u +"$DATE_FORMAT")
  INTERVAL=12
  START_DATETIME=$(date -d "$END_DATETIME -$INTERVAL hours" -u +"$DATE_FORMAT")
  START_EPOCH=$(date -d "$START_DATETIME" +%s)
else
  # when the RRD data is frozen need relevant date range
  START_DATETIME='2010-03-02T04:57:48.126Z'
  END_DATETIME='2010-03-02T05:42:32.733Z'
fi
echo "querying $START_DATETIME ... $END_DATETIME"
TARGET='sample:ClientJobsRunning'
# Copied grom Grafana Simple JSON Datasource dashboard.
# Replaced relative values in range.raw and rangeRaw
#    "raw": {
#      "from": "now-${INTERVAL}h",
#      "to": "now"
#    }
# with copy of range
# removed the .startTime: $START_EPOCH
# argument
PAYLOAD=$(cat << EOF
{
  "app": "dashboard",
  "requestId": "Q100",
  "timezone": "browser",
  "panelId": 2,
  "range": {
    "from": "$START_DATETIME",
    "to": "$END_DATETIME",
    "raw": {
      "from": "$START_DATETIME",
      "to": "$END_DATETIME"
    }
  },
  "rangeRaw": {
    "from": "$START_DATETIME",
    "to": "$END_DATETIME"
  },
  "interval": "2s",
  "intervalMs": 2000,
  "targets": [
    {
      "target": "$TARGET",
      "refId": "A",
      "type": "timeserie"
    }
  ],
  "maxDataPoints": 100,
  "scopedVars": {
    "__interval": {
      "text": "2s",
      "value": "2s"
    },
    "__interval_ms": {
      "text": 2000,
      "value": 2000
    }
  },
  "adhocFilters": []
}
EOF
)

PAYLOAD=$(echo "$PAYLOAD" |jq -c '.')
if $DEBUG ; then
  echo "PAYLOAD=$PAYLOAD"
fi
# compact

# alternatively if jq is not intalled

PAYLOAD=$(echo "$PAYLOAD"|sed 's|\n||g')
LOGFILE="/tmp/a.$$.json"
curl -X POST $BASE_URL/query -d "$PAYLOAD" | jq -c '.' > $LOGFILE
QUERY='.[0].datapoints | length'
if $DEBUG ; then
  ls -l $LOGFILE
fi
NUM_DATAPOINTS=$(cat $LOGFILE |jq -cr "$QUERY")
if [ $? != 0 ] ; then
  echo 'Error detected'
  STATUS=1
fi
if [ "$NUM_DATAPOINTS" = 0 ] ; then
  echo 'jq: No datapoints returned'
  STATUS=2
else
  echo "jq: $NUM_DATAPOINTS datapoints returned"
  STATUS=0
fi
# alternartively run one-liner Python script

SCRIPT=$(cat <<EOF
from __future__ import print_function
import sys
import json
f = open(sys.argv[1])
data = json.load(f)
print ('python: {} datapoints returned'.format(len(data[0]['datapoints'])))
f.close()
EOF

)
python -c "$SCRIPT" $LOGFILE
rm -f $LOGFILE

