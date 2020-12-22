#!/bin/bash

if [[ -z "${DEBUG}" ]] ; then
  echo 'DEBUG was not set explicitly, default is false'
  DEBUG='false'
fi
echo "DEBUG=${DEBUG}"

HOSTNAME=${1:-host1}
QUERY=".[]|select(.type=\"agent\")|select(.name|match(\"$HOSTNAME.\"))|.name"
# for chained calls, '.path' at the end will be more useful

UCD_URL=https://localhost:8443
# read -sp "Enter user: " USERNAME
# read -sp "Enter password: " PASSWORD
# AUTHENTICATION="-u $USERNAME:$PASSWORD" 
#

RESOURCE_ID='/TEST'
TEMP_FILE1=/tmp/a.txt
TEMP_FILE2=/tmp/b.txt
cat /dev/null > $TEMP_FILE2
if [[ "$CALL_API" != "" ]] ; then
  curl -K $AUTHENTICATION "${UCD_URL}/cli/resource/?parent=${RESURCE_ID}" | tee $TEMP_FILE1
else 
  cat <<EOF>$TEMP_FILE1
[ 
  {
    "id": "1122334455-ffff-aaaa-bbbb-10101010",
    "path": "/TEST/host1.domain_use_agent_name",
    "active": true,
    "prototype": false,
    "name": "host1.domain_use_agent_name",
    "description": "ucd agent on host1",
    "hasAgent": true,
    "status": "ONLINE",
    "type": "agent",
    "tags": []
  },
  {
    "id": "1122334455-ffff-aaaa-bbbb-10101010",
    "path": "/TEST/host2.domain_use_agent_name",
    "active": true,
    "prototype": false,
    "name": "host2.domain_use_agent_name",
    "description": "ucd agent on host2",
    "hasAgent": true,
    "status": "ONLINE",
    "type": "agent",
    "tags": []
  }
]
EOF
fi
jq -cr $QUERY < $TEMP_FILE1 | while read DATA; do
  echo $DATA >> $TEMP_FILE2
done
echo "Results in ${TEMP_FILE2}:"
cat $TEMP_FILE2

