#!/bin/bash

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
read -sp "Enter user: " USERNAME
read -sp "Enter password: " PASSWORD
AUTHENTICATION="-u $USERNAME:$PASSWORD"
BASE_URL="http://localhost:8443"

COMPONENT_NAME=${1:-COMPONENT}
echo "COMPONENT_NAME:=${COMPONENT_NAME}"

API="/cli/component/info?component=${COMPONENT_NAME}"
QUERY='.id'
OPTIONS='-cr'
# NOTE: need to avoid '//' from contatinating BASE_URL with API
# see also:
# https://github.com/UrbanCode/uDeployRestClient/blob/master/src/main/java/com/urbancode/ud/client/ComponentClient.java#L299
COMPONENT_ID=$(curl -k $AUTHENTICATION "${BASE_URL}${API}" 2>/dev/null| jq $OPTIONS $QUERY)
echo "COMPONENT_ID:=${COMPONENT_ID}"

TMP_FILE1=/tmp/a$$.json
API="/rest/deploy/component/${COMPONENT_ID}"
QUERY='.id'
OPTIONS='-cr'
curl -k $AUTHENTICATION "${BASE_URL}${API}" 2>/dev/null| jq $OPTIONS $QUERY| tee $TMP_FILE1 > /dev/null

OPTIONS='-cr'
QUERY='.resourceRole|.propDefs|.[]|["name",.name]|join("=")'
NAMES=($(cat $TMP_FILE1| jq $OPTIONS $QUERY))
echo "NAMES (*): ${#NAMES[@]}"

OPTIONS='-cr'
QUERY='.resourceRole|.propSheetDef|.id'
PROPDEF_ID=($(cat $TMP_FILE1 | jq $OPTIONS $QUERY))
echo "PROPDEF_ID (*): ${PROPDEF_ID}"

TMP_FILE2=/tmp/b$$.json
API="/property/propSheetDef/${PROPDEF_ID}/propDefs"
QUERY='.id'
OPTIONS='-cr'
curl -k $AUTHENTICATION "${BASE_URL}${API}" 2>/dev/null| jq $OPTIONS $QUERY| tee $TMP_FILE2 > /dev/null
OPTIONS='-cr'
QUERY='.[]|["name",.name]|join("=")'
NAMES=($(cat $TMP_FILE2| jq $OPTIONS $QUERY))
echo "NAMES (*): ${#NAMES[@]}"
exit 0


