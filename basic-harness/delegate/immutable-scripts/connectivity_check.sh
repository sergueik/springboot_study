#!/bin/bash -e
# Copyright 2021 Harness Inc. All rights reserved.
# Use of this source code is governed by the PolyForm Free Trial 1.0.0 license
# that can be found in the licenses directory at the root of this repository, also available at
# https://polyformproject.org/wp-content/uploads/2020/05/PolyForm-Free-Trial-1.0.0.txt.

function check_connection_with_ca_bundle() {
  if [ -z $1 ]; then
    echo "Checking connection to Harness manager using default ca bundle file"
  else
    echo "Checking connection to Harness manager using ca bundle from $1"
  fi
  command="CURL_CA_BUNDLE=$1 curl --connect-timeout 10 --write-out %{http_code} $MANAGER_PROXY_CURL -s $MANAGER_HOST_AND_PORT/api/account/$ACCOUNT_ID/status  --output /dev/null"
  status=$(eval "$command")
  echo -e "\tHTTP Status Code: $status, Curl exit code  $?"
  [[ $status == 000 ]] && echo -e "\tConnectivity test failed. Please check network connection from your delegate to Harness manager." && return 1
  [[ $status == 401 ]] && echo -e "\tAccount ID: $ACCOUNT_ID not found. Stop launching delegate." && exit 2
  # other errors
  [[ $status -ge 400 ]] && echo -e "\tConnection check to Harness manager failed." && return 3
  return 0
}

# By default, curl takes /etc/pki/tls/certs/ca-bundle.crt as it's ca bundle in RHEL
# curl will take ca bundle provided by CURL_CA_BUNDLE or --cacert argument
RHEL_CA_TRUSTED_BUNDLE_FILE="/etc/pki/tls/certs/ca-bundle.trust.crt"

if [[ $PRECHECK_CONN == "true" ]]; then
  set +e
  set -v
  check_connection_with_ca_bundle
  if [ $? != 0 ]; then
    check_connection_with_ca_bundle "$RHEL_CA_TRUSTED_BUNDLE_FILE"
    if [ $? != 0 ]; then
      echo "Connection check to Harness manager failed. Stop launching delegate."
      exit 4
    fi
  fi
  echo "Connection test is successful."
  set -e
  set +v
fi
