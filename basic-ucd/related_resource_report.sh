#!/bin/bash

# reports one to many resource association 
# e.g. components added to specific agent
# the path of the parent resource serves like the foreign key

if [[ -z "${DEBUG}" ]] ; then
  echo 'DEBUG was not set explicitly, default is false'
  DEBUG='false'
fi
echo "DEBUG=${DEBUG}"
DATA_FILE=/tmp/a.json

cat <<EOF>$DATA_FILE
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
UCD_URL=https://localhost:8443
# read -sp 'Enter user: ' USERNAME
# read -sp 'Enter password: ' PASSWORD
# AUTHENTICATION="-u $USERNAME:$PASSWORD" 

RESULT1_JSON=/tmp/b.json
RESULT_FILE=/tmp/c.txt
RESOURCE_NAME='dummy'
# NOTE: .name,.path will produce two lines
cat $DATA_FILE |jq '.[]|.name + "\" \"" + .path'| while read $RESOURCE_NAME, $RESOURCE_PATH  ; do

  if [[ "${DEBUG}" = 'true' ]] ; then
    # NOTE: $RESULT1_JSON will be overwritten in every iteration
    curl -k $AUTHENTICATION "${UCD_URL}/cli/resource/?parent=${RESOURCE_PATH}" jq '.' | tee $RESULT1_JSON > /dev/null
    curl -k $AUTHENTICATION "${UCD_URL}/cli/resource/?parent=${RESOURCE_PATH}" jq -cd '.[]|.name'
  fi
  # collect all linked resourses - will filter later
  # like to prefix every result row with the
  # $RESOURCE_NAME
  # NOTE: not using the $RESOURCE_PATH:
  # that column can be quote long
  # compose the extra columnt by jq, csv style:
  $QUERY=".[]|\"$RESOURCE_NAME\"+ \",\" .name"
  curl -k $AUTHENTICATION "${UCD_URL}/cli/resource/?parent=${RESOURCE_PATH}" jq -cd "${QUERY}" | tee $RESULT_FILE > /dev/null

done
echo "Raw sample data in ${}"
echo "Results in ${}"
