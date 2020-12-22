#!/bin/bash

# reports one to many resource association 
# e.g. components added to specific agent
# the path of the parent resource serves like the foreign key

if [[ -z "${DEBUG}" ]] ; then
  echo 'DEBUG was not set explicitly, default is false'
  DEBUG='false'
fi
echo "DEBUG=${DEBUG}"
TMP_FILE=/tmp/a.json
# TODO: getopt
RESOUCE_NAME=${1:-TEST}
RESOUCE_ID=$2
APLICATION_PATH='/MAIN_APPLICTION'

# TODO: real urlencode
RESOURCE_PATH=$(cat "${APPLICTION_PATH}/${RESOURCE_NAME}" | sed '|/|%2F|g')
UCD_URL=https://localhost:8443
# read -sp "Enter user: " USERNAME
# read -sp "Enter password: " PASSWORD
# AUTHENTICATION="-u $USERNAME:$PASSWORD" 

# NOTE the REST calls underlying getResourceByPath and getResourceById 
# accepts both id and path argument uniformly
echo 'Expploring resource path: ' $RESOURCE_PATH

curl -k $AUTHENTICATION "${UCD_URL}/rest/resource/resource/${RESOURCE_URL}" | tee $TMP_FILE 1>/dev/null

if [[ "${DEBUG}" = 'true' ]] ; then
  QUERY='.'
  OPTIONS=''
  jq $OPTIONS $QUERY < $TMP_FILE
fi

QUERY='.id'
OPTIONS='-cr'
RESOURCE_ID=$(jq $OPTIONS $QUERY < $TMP_FILE)
echo 'Resource id: ' $RESOURCE_ID
rm -f $TMP_FILE


AGENT_NAME=$3

if [[ !-z $AGENT_NAME ]]; then
  QUERY=".[]|select(.type=\"agent\")|select(.name|test(\"$AGENT_NAME.\"))|.name"
  OPTIONS='-cr'
  echo 'Agent: ' $AGENT_NAME
else	
  QUERY='.[]|select(.type="agent)|.name"'
  OPTIONS='-cr'
  echo 'Agents:'
fi	
jq $OPTIONS $QUERY < $TEMP_FILE | while read DATA; do
  echo $DATA >> $DATA_FILE
done
echo "Results in ${DATA_FILE}:"
cat $DATA_FILE
