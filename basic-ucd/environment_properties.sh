#!/bin/bash

# extract the props that are used in 'Edit' dialog
TMP_FILE=/tmp/a$$.txt
# TODO: provide the subresource REST call to collect these
RESOURCE_ID=''
ROLE_ID=''
# NOTE: response is  slow. Wrap the curl call in a subshell
(
curl -k $AUTHENTICATION "$BASE_URL/rest/resource/resource/${RESOURCE_ID}/propertiesForRole/${ROLE_ID}"  2>&1
) > $TMP_FILE
cat $TMP_FILE |jq '.'
