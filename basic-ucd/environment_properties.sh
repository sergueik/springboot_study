#!/bin/bash
# Steps:
# 1. /cli/resource/parent?=$ENVIRONMENT_RESOURCE_ID => $AGENT_NAME
# 1a (alternative) /rest/resource/resource/$ENVIRONMENT_RESOURCE_ID/resources
# This call is quite universal
# /rest/resource/resource/resources | jq '.[]|.id'  => top level resources
#- can have agents, pools, groups
# 2. /cli/resource/parent?=$AGENT_NAME_URLECODED => $COMPONENT_ID based on $COMPONENT_NAME
# 3. /rest/resource/resource/$COMPONENT_ID/roles => propDefs

# 4. /rest/resource/resource/$COMPONENT_ID => JSON with several useful nodes:
# QUERY='.role,.roleProperties'
# OPTIONS=
# so extract ROLE_ID
# QUERY='.role|.id'
# to extract ROLE_PROPERTIES from component resource
# QUERY='.roleProperties|to_entries[]' 
# NOTE: there is a special API for the same. It needs component id and role id
# getResourceRoleProperties
# see also: 
# https://github.com/UrbanCode/uDeployRestClient/blob/master/src/main/java/com/urbancode/ud/client/ResourceClient.java#L379
#
UCD_URL=https://localhost:8443
# read -sp 'Enter user: ' USERNAME
# read -sp 'Enter password: ' PASSWORD
# AUTHENTICATION="-u $USERNAME:$PASSWORD" 
# AUTHENTICATION_DISPLAY="-u '$USERNAME:$PASSWORD'" 

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
OPTIONS='-cr'
QUERY=".[]|select(.name|test(\"${COMPONENT_NAME}\"))|.id"
COMPONENT_ID=$(
jq $OPTIONS $QUERY < $TMP_FILE | while read DATA ; do
  echo $DATA
done
)
QUERY='.roleProperties|to_entries[]'
PARAMS=$(
jq $OPTIONS $QUERY < $TMP_FILE | jq --splurp '.[]|.key,.value' |while read DATA ; do
  echo $DATA
done
)
