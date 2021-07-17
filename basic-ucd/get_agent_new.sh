#!/bin/bash

# for base64 one pipes the data e.g.: 
# $(echo -n "example=$1&log=$result" | base64)

rawurlencode() {
# based on: https://gist.github.com/moyashi/4063894
# see also https://stackoverflow.com/questions/32273446/encode-url-variable-with-curl

local INPUT_STRING="${1}"
# NOTE debug printout need to be redirected to STDERR
# 1>&2 echo "Processing $INPUT_STRING"
INPUT_STRING_URLENCODED=$(
awk -v VAR="$INPUT_STRING" -e ' BEGIN{ for (I = 0; I <= 255; I++) { ord[sprintf("%c", I)] = I; } } function escape(DATA, c, len, res) { len = length(DATA) ; res = ""; for (i = 1; i <= len; i++) { c = substr(DATA, i, 1); if(c ~ /[0-9A-Za-z]/){ res = res c; } else { res = res "%" sprintf("%02X", ord[c]); } } return res; } END{ print escape(VAR); }' /dev/null
)
echo $INPUT_STRING_URLENCODED
}
# Auth
read -sp 'Enter user: ' USERNAME
read -sp 'Enter password: ' PASSWORD
AUTHENTICATION="-u $USERNAME:$PASSWORD"
BASE_URL="http://localhost:8443"

# Agent id by name - not very practical
AGENT_NAME=${1:-Agent_Name}
echo "AGENT_NAME: ${AGENT_NAME}"
# NOTE: not very practical
API="/cli/agentCLI/info?agent=${AGENT_NAME}"
QUERY='.id'
OPTIONS='-cr'
# NOTE: need to avoid '//' from contatinating BASE_URL with API
# see also:
# https://github.com/UrbanCode/uDeployRestClient/blob/master/src/main/java/com/urbancode/ud/client/ComponentClient.java#L299
AGENT_ID=$(curl -k $AUTHENTICATION "${BASE_URL}${API}" 2>/dev/null| jq $OPTIONS $QUERY)
echo "AGENT_ID (*): ${AGENT_ID}"

# Agent from environment and partial agent name. Note environments are also
# possible to retrieve from application
ENVIRONMENT_NAME=${2:-Environment}
echo "ENVIRONMENT_NAME: ${ENVIRONMENT_NAME}"
API="/cli/environment/info?environment=${ENVIRONMENT_NAME}"
OPTIONS='-cr'
QUERY='.id'
ENVIRONMENT_ID=$(curl -k $AUTHENTICATION "${BASE_URL}${API}" 2>/dev/null| jq $OPTIONS $QUERY)
echo "ENVIRONMENT_ID (*): ${ENVIRONMENT_ID}"

# Agent resources in the enviornment
API="/cli/resource/?parent=${ENVIRONMENT_ID}"
OPTIONS='-cr'
QUERY='.id'
TMP_FILE1=/tmp/a$$.json
curl -k $AUTHENTICATION "${BASE_URL}${API}" 2>/dev/null| jq $OPTIONS $QUERY| tee $TMP_FILE1 > /dev/null

# NOTE: globbing
OPTIONS='-cr'
QUERY=".[]|select(.type=\"agent\")|select(.name|test(\"$AGENT_NAMEi.*\"))|.path"
RESULT=$(
cat $TMP_FILE1| jq $OPTIONS $QUERY| while read DATA ; do
  echo $DATA
done
)
# Note: getting the path, not the id
AGENT_PATH=$(rawurlencode $RESULT)
echo "AGENT_PATH (*): ${AGENT_PATH}"

# Component resource sub-resources
API="/cli/resource/?parent=${AGENT_PATH}"
OPTIONS='-cr'
QUERY='.id'
TMP_FILE2=/tmp/b$$.json
curl -k $AUTHENTICATION "${BASE_URL}${API}" 2>/dev/null| jq $OPTIONS $QUERY| tee $TMP_FILE2 > /dev/null


COMPONENT_RESOURCE_NAME=${3:-Component_Resource}
OPTIONS='-cr'
QUERY=".[]|select(.name|test(\"$COMPONENT_RESOUCE_NAME\"))|.id"
COMPONENT_RESOURCE_ID=($(cat $TMP_FILE2 | jq $OPTIONS $QUERY))
echo "COMPONENT_RESOURCE_ID (*): ${COMPONENT_RESOURCE_ID}"

API="/rest/resoure/resource/${COMPONENT_RESOURCE_ID}/roles"
OPTIONS='-cr'
QUERY='.id'
TMP_FILE3=/tmp/c$$.json
curl -k $AUTHENTICATION "${BASE_URL}${API}" 2>/dev/null| jq $OPTIONS $QUERY| tee $TMP_FILE3 > /dev/null
OPTIONS='-cr'
QUERY='.[]|.propDefs|.[]["name",.name]|join("=")'
NAMES=($(cat $TMP_FILE3| jq $OPTIONS $QUERY))
echo "NAMES (*): ${#NAMES[@]}"

API="/rest/resource/resource/${COMPONENT_RESOURCE_ID}"
OPTIONS='-cr'
QUERY='.id'
TMP_FILE4=/tmp/d$$.json
curl -k $AUTHENTICATION "${BASE_URL}${API}" 2>/dev/null| jq $OPTIONS $QUERY| tee $TMP_FILE4 > /dev/null

OPTIONS='-cr'
QUERY=".role|.id"
ROLE_ID=$(
cat $TMP_FILE1| jq $OPTIONS $QUERY| while read DATA ; do
  echo $DATA
done
echo "ROLE_ID (*): ${ROLE_ID}"
)

API="/rest/resource/resource/${COMPONENT_RESOURCE_ID}/propertiesForRole/${ROLE_ID}"
OPTIONS='-cr'
QUERY='.id'
TMP_FILE5=/tmp/e$$.json
(
  curl -k $AUTHENTICATION "${BASE_URL}${API}" 2>/dev/null| jq $OPTIONS $QUERY
)| tee $TMP_FILE5 > /dev/null

OPTIONS='-cr'
QUERY='.[]|.propDefs|.[]["name",.name]|join("=")'
NAMES=($(cat $TMP_FILE5| jq $OPTIONS $QUERY))
echo "NAMES (*): ${#NAMES[@]}"

OPTIONS='-cr'
QUERY='.[]|.propValue|.[]["value",.value]|join("=")'
VALUES=($(cat $TMP_FILE5| jq $OPTIONS $QUERY))
echo "VALUES (*): ${#VALUES[@]}"

TMP_INPUT_FILE=/tmp/f$$.json
cat <<EOF> $TMP_INPUT_FILE
{
  "param1": "value1",
  "param2": "value2",
  "param3": "value3"
}
EOF

API="/rest/resource/resource/${COMPONENT_RESOURCE_ID}/savePropertiesForRole/${ROLE_ID}"
OPTIONS='-cr'
QUERY='.id'
curl -k $AUTHENTICATION "${BASE_URL}${API}" -d $(cat $TMP_INPUT_FILE | jq -c '.') 2>/dev/null


exit 0


