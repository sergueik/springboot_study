#!/bin/sh

RANGE=${1:-current}
if [ "$RANGE" =  'current' ]; then
  # when RRD feed s live the health check needs to query the recent dates
  # and be consistent with the format
  
  DATE_FORMAT='%Y-%m-%dT%H%M%SZ'
  DATE_TO=$(date +"$DATE_FORMAT")
  INTERVAL=12
  DATE_FROM=$(date -d "$DATE_TO -$INTERVAL hours")
else
  # when the RRD data is frozen need relevant date range
  DATE_FROM='2010-03-02T04:57:48.126Z'
  DATE_TO='2010-03-02T05:42:32.733Z'
fi

TARGET='sample:ClientJobsRunning'
PAYLOAD=$(cat << EOF
{
  "timezone": "browser",
  "panelId": 2,
  "range": {
    "from": "$DATE_FROM",
    "to": "$DATE_TO",
    "raw": {
      "from": "$DATE_FROM",
      "to": "$DATE_TO"
    }
  },
  "rangeRaw": {
    "from": "$DATE_FROM",
    "to": "$DATE_TO"
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
  "maxDataPoints": 928,
  "scopedVars": {
    "__interval": {
      "text": "2s",
      "value": "2s"
    },
    "__interval_ms": {
      "text": 2000,
      "value": 2000
    }
  }
}
EOF
)

PAYLOAD=$(echo "$PAYLOAD"|jq -c)
# compact

# alternatively if jq is not intalled

PAYLOAD=$(echo "$PAYLOAD"|sed 's|\n||g')
LOGFILE="/tmp/a.$$.json"
curl -X POST http://localhost:9001/query -d "$PAYLOAD" > $LOGFILE
QUERY=".[0].datapoints | length"
NUM_DATAPOINTS=$(cat $LOGFILE |jq -cr $QUERY)
if [ $? ne 0 ] ; then
  echo 'Error detected'
  STATUS=1
fi
if [ "$NUM_DATAPOINTS" = 0 ] ; then
  echo 'No datapoints returned'
  STATUS=2
else
  echo "$NUM_DATAPOINTS datapoints returned"
  STATUS=0
fi
