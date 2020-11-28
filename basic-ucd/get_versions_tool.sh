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

SNAPSHOT_API_RESPONSE='snapshot.json'
echo "Processing API response file ${SNAPSHOT_API_RESPONSE}"

SNAPSHOT_COMPONENT_NAMES=($(jq '.[]|.name' $SNAPSHOT_API_RESPONSE|jq -cr '.'))
RESULT_FILE='snapshot_components.txt'
echo "All snapshot components in ${RESULT_FILE}"
(
for NAME in "${SNAPSHOT_COMPONENT_NAMES[@]}"; do echo $NAME; done
) |sort|tee $TMP_FILE >/dev/null
mv $TMP_FILE $RESULT_FILE
# component names with embedded desiredVersions document
VERSIONED_COMPONENT_NAMES=($(jq '.[]|select( .desiredVersions|length == 1)|.name' $SNAPSHOT_API_RESPONSE|jq -cr '.'))
RESULT_FILE='versioned_components.txt'
echo "Versioned components in ${RESULT_FILE}"

(
for NAME in "${VERSIONED_COMPONENT_NAMES[@]}"; do echo $NAME; done
) |sort|tee $TMP_FILE >/dev/null

mv $TMP_FILE $RESULT_FILE

# 0 for version-less components
VERSIONLESS_COMPONENT_NAMES=($(jq '.[]|select( .desiredVersions|length == 0)|.name' $SNAPSHOT_API_RESPONSE|jq -cr '.'))

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
for NAME in "${RELEASE_COMPONENT_NAMES[@]}";
do
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
for NAME in "${RELEASE_COMPONENT_NAMES[@]}";
do
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
for NAME in "${RELEASE_COMPONENT_NAMES[@]}";
do
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


