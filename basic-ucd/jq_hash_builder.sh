#!/bin/bash
HOSTNAME=${1:-host1}
# ini format
UCD_URL='https://localhost:8443'
RESOURCE_ID='/TEST'
TEMP_FILE=/tmp/a.txt
if [[ "$CALL_API" != "" ]] ; then
  curl -K $AUTH "$UCD_URL/cli/resource/?parent=$RESURCE_ID" | tee $RESULT_FILE1
else
  cat <<EOF>$TEMP_FILE
[
  {
    "id": "170f4303-0ddd-abbd-3f02-03789d3cdd9d",
    "created": 1593195086676,
    "securityResourceId": "1730c27e-ba73-5ee2-e8ec-25e686478278",
    "name": "component_1",
    "description": "component 1 description",
    "componentType": "STANDARD",
    "active": true,
    "deleted": false,
    "integrationFailed": false,
    "defaultVersionType": "FULL",
    "tags": [],
    "user": "user@domain.com",
    "desiredVersions": [],
    "templateVersion": -1,
    "cleanupDaysToKeep": 0,
    "cleanupCountsToKeep": 0,
    "templateTags": [],
    "children": false
  },
  {
    "id": "172f1d3c-3650-453c-0c06-279d5cbb3582",
    "created": 1593195086676,
    "securityResourceId": "172f1d3c-35ce-8c9b-7ddd-17b769ceb2f1",
    "name": "component_3",
    "description": "component 3 description",
    "componentType": "STANDARD",
    "active": true,
    "deleted": false,
    "integrationFailed": false,
    "defaultVersionType": "FULL",
    "tags": [],
    "user": "user@domain.com",
    "desiredVersions": [
      {
        "id": "172f1d3c-35f2-7aa1-a9a3-d2fb56513b79",
        "securityResourceId": "172f1d3c-35ce-8c9b-7ddd-17b769ceb2f1",
        "name": "version_3",
        "description": "version 3 description",
        "type": "FULL",
        "created": 1593195086676,
        "componentType": "STANDARD",
        "sizeOnDisk": 60743680,
        "importing": false,
        "archived": false,
        "useVfs": true,
        "active": true,
        "creator": "process name",
        "component": {
          "id": "172f1d3c-3650-453c-0c06-279d5cbb3582",
          "created": 1593195086676,
          "securityResourceId": "172f1d3c-35ce-8c9b-7ddd-17b769ceb2f1",
          "name": "component_3",
          "description": "component 3 description",
          "componentType": "STANDARD",
          "active": true,
          "deleted": false,
          "integrationFailed": false,
          "defaultVersionType": "FULL",
          "tags": [],
          "user": "user@domain.com",
          "templateTags": [],
          "security": {
            "read": true,
            "View component": true,
            "Manage Versions": true,
            "Manage Properties": true,
            "Delete": true,
            "Create components": true
          }
        },
        "statuses": []
      }
    ],
    "templateVersion": -1,
    "cleanupDaysToKeep": 0,
    "cleanupCountsToKeep": 0,
    "templateTags": [],
    "children": false
  }
]

EOF

fi
# ini style
RESULT_FILE1=/tmp/1.txt
QUERY1=".[]|\"name=\" + \"\\\"\" + .name + \"\\\"\" + \" description=\" + \"\\\"\" + .description +  \"\\\"\" "
echo ${QUERY1}
cat /dev/null > $RESULT_FILE1
jq -cr "${QUERY1}" < $TEMP_FILE | while read DATA; do
  echo $DATA >> $RESULT_FILE1
done
echo "Results in ${RESULT_FILE1}:"
cat $RESULT_FILE1
# name="component_one" description="component 1 description"


# csv style for old shell hash-like functionality
RESULT_FILE2=/tmp/2.txt
QUERY2=".[]| .name + \":\" + .description"
echo ${QUERY2}
cat /dev/null > $RESULT_FILE2
jq -cr "${QUERY2}" < $TEMP_FILE | while read DATA; do
  echo $DATA >> $RESULT_FILE2
done
echo "Results in ${RESULT_FILE2}:"
cat $RESULT_FILE2
cat $RESULT_FILE2 | while read ENTRY ; do
  # echo processing $ENTRY
  KEY="${ENTRY%%:*}"
  # echo processing $KEY
  VALUE="${ENTRY##*:}"
  # echo processing $VALUE
  printf "data [\"%s\"]=\"%s\"\n" "${KEY}" "${VALUE}"
done

# using quotes to tokenize
RESULT_FILE3=/tmp/3.txt
QUERY3=".[]| \"\\\"\" + .name + \"\\\"  \\\"\" + .description + \"\\\"\""
echo ${QUERY3}
cat /dev/null > $RESULT_FILE3
jq -cr "${QUERY3}" < $TEMP_FILE | while read DATA; do
  echo $DATA >> $RESULT_FILE3
done
echo "Results in ${RESULT_FILE3}:"
cat $RESULT_FILE3
cat $RESULT_FILE3 | while read KEY VALUE ; do
  KEY=$(echo $KEY|sed 's|^"||;s|"$||')
  VALUE=$(echo $VALUE|sed 's|^"||;s|"$||')
  printf "data [\"%s\"]=\"%s\"\n" "${KEY}" "${VALUE}"
done

# using quotes to tokenize (let jq do the quoting)
RESULT_FILE4=/tmp/4.txt
QUERY4=".[]|.name + \"\\\" \\\"\" + .description"
echo ${QUERY4}
cat /dev/null > $RESULT_FILE4
jq "${QUERY4}" < $TEMP_FILE | while read DATA; do
  echo $DATA >> $RESULT_FILE4
done
echo "Results in ${RESULT_FILE4}:"
cat $RESULT_FILE4
cat $RESULT_FILE4 | while read KEY VALUE ; do
  KEY=$(echo $KEY|sed 's|^"||;s|"$||')
  VALUE=$(echo $VALUE|sed 's|^"||;s|"$||')
  printf "data [\"%s\"]=\"%s\"\n" "${KEY}" "${VALUE}"
done
exit 0
