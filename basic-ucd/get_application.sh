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
# USERNAME=$(whoami)
if [ -z "$AUTHENTICATION" ] ; then
  if [ -z "$USERNAME" ] ; then
    # NOTE: does not preserve new lines
    read -sp 'Enter user: ' USERNAME
  fi
  if [ -z "$PASSWORD" ] ; then
    echo ''
    echo -n 'Enter password:'
    read -s PASSWORD
    echo ''
  fi
AUTHENTICATION="-u $USERNAME:$PASSWORD"
fi 
BASE_URL='https://localhost:8443'
APPLICATION_NAME=${1:-Test_Application}
echo "APPLICATION_NAME:=${APPLICATION_NAME}"
API="/cli/application/info?application=${APPLICATION_NAME}"
OPTIONS='-cr'
QUERY='.id'
# NOTE: pay attention to avoid '//' after contatinating $BASE_URL with $API
APPLICATION_ID=$(curl -k $AUTHENTICATION "${BASE_URL}${API}" 2>/dev/null| jq $OPTIONS $QUERY)
echo "APPLICATION_ID (*): ${APPLICATION_ID}"


# save the full JSON in the temp file
TMP_FILE1=/tmp/a$$.json
API="/cli/application/environmentsInApplication?application=${APPLICATION_NAME}"
OPTIONS='-cr'
QUERY='.'
curl -k $AUTHENTICATION "${BASE_URL}${API}" 2>/dev/null| jq $OPTIONS $QUERY| tee $TMP_FILE1 > /dev/null
# process data using bash syntax
OPTIONS='-cr'
QUERY='.[]|["id",.id]|join("=")'
IDS=($(cat $TMP_FILE1 | jq $OPTIONS $QUERY))
echo "IDS=${IDS[@]}"
OPTIONS='-cr'
QUERY='.[]|["name",.name]|join("=")'
# using regular shell syntax
NAMES=''
cat $TMP_FILE1| jq $OPTIONS "${QUERY}"| while read LINE ; do
  # echo $LINE
  NAMES=$(echo ${NAMES} ${LINE})
  # echo "NAMES=${NAMES}"
done
# TODO: variable value is lost outside of the loop
# echo "NAMES=${NAMES}"

# using bash syntax
NAMES=($(cat $TMP_FILE1| jq $OPTIONS $QUERY))
echo "NAMES=${NAMES[@]}"
MAX_INDEX="${#NAMES[@]}"

echo 'JSON'

ARGLINE=''
QUERY='.'
for((CNT=0;CNT<$MAX_INDEX;CNT++)) ; do
  NAME=$(echo ${NAMES[$CNT]} | cut -d '=' -f 2)
  VALUE=$(echo ${IDS[$CNT]} | cut -d '=' -f 2)
  # stop processing of entries with no value/ blank value
  if [[ -z $VALUE ]]  ; then
    continue
  fi
  # limit the subsequent queries to the specific enironment
  # this relies on UCD idiosyncracies in environment namings
  if echo $NAME| grep -iEqv '^(dev|test|uat)\-(east|west)$' ; then
    continue
  fi

  ARGLINE="${ARGLINE} --arg name${CNT} ${NAME} --arg val${CNT} ${VALUE}"
  QUERY="${QUERY} |.[\$name${CNT}]=\$val${CNT}"
done

jq $ARGLINE "$QUERY" <<<'{}'

# 1>2 echo 'CSV:'
echo 'CSV:'
for CNT in $(seq 0 $MAX_INDEX) ; do
  NAME=$(echo ${NAMES[$CNT]} | cut -d '=' -f 2)
  VALUE=$(echo ${IDS[$CNT]} | cut -d '=' -f 2)
  # stop processing of entries with no value/ blank value
  if [[ -z $VALUE ]]  ; then
    continue
  fi
  # additional REST calls
  # it will return JSON with application GUID 
  # but without the environment resource GUID
  API="/rest/deploy/environment/${VALUE}"
  # echo "API=${API}"
  OPTIONS=''
  QUERY='.application|.id'
  TMP_FILE2=/tmp/b$$.json
  # curl -k $AUTHENTICATION "${BASE_URL}${API}"
  curl -k $AUTHENTICATION "${BASE_URL}${API}" 2>/dev/null| jq $OPTIONS $QUERY| tee $TMP_FILE2 > /dev/null
  APPLICATION_GUID=$(cat $TMP_FILE2)
  echo "APPLICATION_GUID=${APPLICATION_GUID}"
  API="/rest/deploy/environment/${VALUE}/resources"
  # echo "API=${API}"
  OPTIONS='-cr'
  QUERY='.[]|.id'
  # NOTE: the response may be empty - it will find top level group in the environment after it is added via 'Add Base Resource' 
  # 'Environment: TEST-WEST for Test_Application '
  TMP_FILE3=/tmp/c$$.json
  curl -k $AUTHENTICATION "${BASE_URL}${API}" 2>/dev/null| jq $OPTIONS $QUERY| tee $TMP_FILE3 > /dev/null
  # cat $TMP_FILE3
  ENVIRONMENT_GUID=$(cat $TMP_FILE3)
  # echo "Environment resource GUID=${ENVIRONMENT_GUID}"
  echo -e "$NAME\t$VALUE\t$ENVIRONMENT_GUID"
done

exit 0


