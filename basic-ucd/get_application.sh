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
read -sp 'Enter user: ' USERNAME
read -sp 'Enter password: ' PASSWORD
AUTHENTICATION="-u $USERNAME:$PASSWORD"
BASE_URL='http://localhost:8443'
APPLICATION_NAME=${1:-APPLICATION}
echo "APPLICATION_NAME:=${APPLICATION_NAME}"
API="/cli/application/info?application=${APPLICATION_NAME}"
OPTIONS='-cr'
QUERY='.id'
# NOTE: pay attention to avoid '//' after contatinating $BASE_URL with $API
APPLICATION_ID=$(curl -k $AUTHENTICATION "${BASE_URL}${API}" 2>/dev/null| jq $OPTIONS $QUERY)
echo "APPLICATION_ID (*): ${APPLICATION_ID}"

TMP_FILE1=/tmp/a$$.json
API="/cli/application/environmentsInApplication?application=${APPLICATION_NAME}"
curl -k $AUTHENTICATION "${BASE_URL}${API}" 2>/dev/null| jq $OPTIONS $QUERY| tee $TMP_FILE1 > /dev/null
OPTIONS='-cr'
QUERY='.[]|["id",.id]|join("=")'
# using bash syntax
VALUES=($(cat $TMP_FILE1 | jq $OPTIONS $QUERY))

OPTIONS='-cr'
QUERY='.[]|["name",.name]|join("=")'
# using regular shell syntax
NAMES=''
cat $TMP_FILE1| jq $OPTIONS $QUERY| while read LINE ; do
  NAMES="${NAMES} ${LINE}"
done

# using bash syntax
NAMES=($(cat $TMP_FILE1| jq $OPTIONS $QUERY))
MAX_INDEX="${#NAMES[@]}"

# 1>2 echo 'CSV:'
echo 'CSV:'
for CNT in $(seq 0 $MAX_INDEX) ; do
  NAME=$(echo ${NAMES[$CNT]} | cut -d '=' -f 2)
  VALUE=$(echo ${IDS[$CNT]} | cut -d '=' -f 2)
  # possible to execute the following
  # it will return JSON with application GUID but not with the environment resource GUIT
  API="/rest/deploy/environment/${VALUE}"
  OPTIONS='-cr'
  QUERY='.application|.id'
  TMP_FILE2=/tmp/b$$.json
  curl -k $AUTHENTICATION "${BASE_URL}${API}" 2>/dev/null| jq $OPTIONS $QUERY| tee $TMP_FILE2 > /dev/null
  cat $TMP_FILE2
  API="/rest/deploy/environment/${VALUE}/resources"
  OPTIONS='-cr'
  QUERY='.[]|.id'
  TMP_FILE3=/tmp/c$$.json
  curl -k $AUTHENTICATION "${BASE_URL}${API}" 2>/dev/null| jq $OPTIONS $QUERY| tee $TMP_FILE3 > /dev/null

  ENVIREONMENT_RESOURCE_GUID=cat $TMP_FILE3
  # echo "Environment resource GUID=${ENVIREONMENT_RESOURCE_GUID}"
  echo -e "$NAME\t$VALUE\t$ENVIREONMENT_RESOURCE_GUID"
done
echo 'JSON'

ARGLINE=''
QUERY='.'
for((CNT=0;CNT<$MAX_INDEX;CNT++)) ; do
  NAME=$(echo ${NAMES[$CNT]} | cut -d '=' -f 2)
  VALUE=$(echo ${IDS[$CNT]} | cut -d '=' -f 2)
  if [[ -z $VALUE ]]  ; then
    continue
  fi

  ARGLINE="${ARGLINE} --arg name${CNT} ${NAME} --arg val${CNT} ${VALUE}"
  QUERY="${QUERY} |.[\$name${CNT}]=\$val${CNT}"
done

# echo jq $ARGLINE "'$QUERY'" <<<'{}'
jq $ARGLINE "$QUERY" <<<'{}'

exit 0


