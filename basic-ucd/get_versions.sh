#!/bin/sh
UCD_URL=https://localhost:8443
#
curl -k $AUTH "$UCD_URL/rest/deploy/component/$COMPONENT_NAME/versions/false"
# NOTE:
# parameter naming
# "packing" filterFields parameter values
# into following parameter names
# filterValue_XX,filterType_XX,filtetClass_XX
curl -k $AUTH "$UCD_URL/rest/deploy/version?rowsPerPage=10&pageNumber=1&orderField=dateCreated&sortType=desc&filterFields=component.id&filterFields=active&filterValue._component.id=$COMPONENT_ID&filterType_component.id=eq&fiterClass_component.id=UUID&filterValue_active=true&filterType_active=eq&filterClass_active=Boolean&outputType=BASIC&outputType=LINKED" | jq '.' | tee $TMP_FILE
exit 0
