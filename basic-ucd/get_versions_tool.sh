#!/bin/bash

# extract RELEASE_COMPONENT_NAMES into bash array variable from release descriptor custom JSON rowset keys
DEFAULT_RELEASE_DESCRIPTOR='release.json'
RELEASE_DESCRIPTOR=${1-$DEFAULT_RELEASE_DESCRIPTOR}
echo "Processing release descriptor file ${RELEASE_DESCRIPTOR}"
RELEASE_COMPONENT_NAMES=($(jq -r '.versions|flatten' $RELEASE_DESCRIPTOR|jq -r '[.[]|keys]'|jq -cr 'flatten|.[]'))
TMP_FILE=/tmp/a$$.txt
RESULT_FILE='release_components.txt'
echo "Release components in ${RESULT_FILE}"
(
for NAME in "${RELEASE_COMPONENT_NAMES[@]}"; do echo $NAME; done
) |sort|tee $TMP_FILE >/dev/null
mv $TMP_FILE $RESULT_FILE

# read -sp "Enter user: " USERNAME
# read -sp "Enter password: " PASSWORD
# AUTHENTICATION="-u $USERNAME:$PASSWORD" 
# BASE_URL="http://localhost:8443"
# curl -k $AUTHENTICATION "$BASE_URL/rest/deploy/application/${APPLICATION}/snapshots/false" | jq '.' | tee $TMP_FILE > /dev/null
SNAPSHOTS_API_RESPONSE='snapshots.json'
echo "Processing API response file ${SNAPSHOTS_API_RESPONSE}"

# TODO: use --data-urlencode
# curl -k $AUTHENTICATION "$BASE_URL/cli/snapshot/getSnapshotVersions?application=${APPLICATION}&snapshot=${SNAPSHOT}" | jq '.' | tee $TMP_FILE > /dev/null
SNAPSHOT_API_RESPONSE='snapshot.json'
echo "Processing API response file ${SNAPSHOT_API_RESPONSE}"

SNAPSHOT_COMPONENT_NAMES=($(jq -cr '.[]|.name' $SNAPSHOT_API_RESPONSE))
RESULT_FILE='snapshot_components.txt'
echo "All snapshot components in ${RESULT_FILE}"
(
for NAME in "${SNAPSHOT_COMPONENT_NAMES[@]}"; do echo $NAME; done
) |sort|tee $TMP_FILE >/dev/null
mv $TMP_FILE $RESULT_FILE
# component names with embedded desiredVersions document
VERSIONED_COMPONENT_NAMES=($(jq -cr '.[]|select( .desiredVersions|length == 1)|.name' $SNAPSHOT_API_RESPONSE))
RESULT_FILE='versioned_components.txt'
echo "Versioned components in ${RESULT_FILE}"

(
for NAME in "${VERSIONED_COMPONENT_NAMES[@]}"; do echo $NAME; done
) |sort|tee $TMP_FILE >/dev/null

mv $TMP_FILE $RESULT_FILE

# 0 for version-less components
VERSIONLESS_COMPONENT_NAMES=($(jq -cr '.[]|select( .desiredVersions|length == 0)|.name' $SNAPSHOT_API_RESPONSE))

# https://stackabuse.com/array-loops-in-bash/
# https://tldp.org/LDP/abs/html/subshells.html
RESULT_FILE='versionless_components.txt'
echo "Version-less components in ${RESULT_FILE}"
(
for NAME in "${VERSIONLESS_COMPONENT_NAMES[@]}"; do echo $NAME; done
) |sort|tee $TMP_FILE >/dev/null

mv $TMP_FILE $RESULT_FILE

echo "Check of release components missing from snapshot"
RESULT_FILE='snapshot_components.txt'
(
for NAME in "${RELEASE_COMPONENT_NAMES[@]}"; do
  if ! grep -q $NAME $RESULT_FILE; then
    echo $NAME
  fi
done
) |sort|tee $TMP_FILE >/dev/null
RESULT_FILE='check1.txt'
mv $TMP_FILE $RESULT_FILE
echo "Results in ${RESULT_FILE}"
cat $RESULT_FILE

echo "Check of release components with missing snapshot versions"
RESULT_FILE='versionless_components.txt'
(
for NAME in "${RELEASE_COMPONENT_NAMES[@]}"; do
  if grep -q $NAME $RESULT_FILE; then
    echo $NAME
  fi
done
) |sort|tee $TMP_FILE >/dev/null
RESULT_FILE='check2.txt'
mv $TMP_FILE $RESULT_FILE
echo "Results in ${RESULT_FILE}"
cat $RESULT_FILE

echo "Check of release components with existing snapshot versions"
RESULT_FILE='versioned_components.txt'
(
for NAME in "${RELEASE_COMPONENT_NAMES[@]}"; do
  if grep -q $NAME $RESULT_FILE; then
    echo $NAME
  fi
done
) |sort|tee $TMP_FILE >/dev/null
RESULT_FILE='check3.txt'
mv $TMP_FILE $RESULT_FILE
echo "Results in ${RESULT_FILE}"
cat $RESULT_FILE
exit 0


UCD_URL=https://localhost:8443
#
curl -k $AUTH "$UCD_URL/rest/deploy/component/$COMPONENT_NAME/versions/false"
# NOTE:
# parameter naming
# "packing" filterFields parameter values
# into following parameter names
# filterValue_XX,filterType_XX,filtetClass_XX
COMPONENT_ID=''
curl -k $AUTH "$UCD_URL/rest/deploy/version?rowsPerPage=10&pageNumber=1&orderField=dateCreated&sortType=desc&filterFields=component.id&filterFields=active&filterValue._component.id=$COMPONENT_ID&filterType_component.id=eq&fiterClass_component.id=UUID&filterValue_active=true&filterType_active=eq&filterClass_active=Boolean&outputType=BASIC&outputType=LINKED" | jq '.' | tee $TMP_FILE
exit 0

# based on: https://gist.github.com/moyashi/4063894
# see also https://stackoverflow.com/questions/32273446/encode-url-variable-with-curl 
awk -v VAR='a-$%/\\' -e ' BEGIN{ for (I = 0; I <= 255; I++) { ord[sprintf("%c", I)] = I; } } function escape(DATA, c, len, res) { len = length(DATA) ; res = "" ; for (i = 1; i <= len; i++) { c = substr(DATA, i, 1); if (c ~ /[0-9A-Za-z]/)  { res = res c ; } else {  res = res "%" sprintf("%02X", ord[c]) ; } } return res ; }  END{ print escape(VAR); }'  /dev/null
