#!/bin/bash
# see also: 
# https://github.com/UrbanCode/uDeployRestClient/blob/master/src/main/java/com/urbancode/ud/client/ResourceClient.java#L379
#
UCD_URL=https://localhost:8443
# read -sp "Enter user: " USERNAME
# read -sp "Enter password: " PASSWORD
# AUTHENTICATION="-u $USERNAME:$PASSWORD" 

# Extract the parameters of in 'Edit' dialog shown in resource tree
TMP_FILE=/tmp/a$$.txt
# TODO: provide the subresource REST  to collect these 
RESOURCE_ID=''
ROLE_ID=''
# NOTE: Wraped the slow response curl call in a subshell
(
  curl -k $AUTHENTICATION "${UCD_URL}/rest/resource/resource/${RESOURCE_ID}//propertiesForRole/${ROLE_ID}" 2>&1
) > $TMP_FILE
sleep 1
cat $TMP_FILE |jq '.'

# getResourceRoleProperties