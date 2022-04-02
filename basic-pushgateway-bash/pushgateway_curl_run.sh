#!/usr/bin/env bash
# origin: https://github.com/b4fun/pushgateway-curl/blob/master/run.sh

set -o errexit
set -o nounset
set -o pipefail

# env:VERBOSE: when set to yes, enable shell trace
# NOTE: set trace enabled, there is potential to leak the auth secrets :)
if [[ "${VERBOSE:-""}" == "yes" ]]; then
    set -o xtrace
fi

# env,argument:PUSHGATEWAY_METRICS_URL: the metrics url to push to
PUSHGATEWAY_METRICS_URL="${1:-"${PUSHGATEWAY_METRICS_URL:-""}"}"
# env,argument:PUSHGATEWAY_HOST: pushgateway host to override in the request
PUSHGATEWAY_HOST="${2:-"${PUSHGATEWAY_HOST:-""}"}"
# env:PUSHGATEWAY_BASIC_AUTH_USERNAME: pushgateway basic auth username
PUSHGATEWAY_BASIC_AUTH_USERNAME="${PUSHGATEWAY_BASIC_AUTH_USERNAME:-""}"
# env:PUSHGATEWAY_BASIC_AUTH_PASSWORD: pushgateway basic auth password
PUSHGATEWAY_BASIC_AUTH_PASSWORD="${PUSHGATEWAY_BASIC_AUTH_PASSWORD:-""}"
# env:PUSHGATEWAY_METHOD: request method to use, defaults to PUT
PUSHGATEWAY_METHOD="${PUSHGATEWAY_METHOD:-"PUT"}"

# env:METRIC_SOURCE_URL: metric source url to read from
METRIC_SOURCE_URL="${METRIC_SOURCE_URL:-""}"
# env:METRIC_SOURCE_HOST: metric source host to override in the request
METRIC_SOURCE_HOST="${METRIC_SOURCE_HOST:-""}"
# env:METRIC_SOURCE_BASIC_AUTH_USERNAME: metric source basic auth username
METRIC_SOURCE_BASIC_AUTH_USERNAME="${METRIC_SOURCE_BASIC_AUTH_USERNAME:-""}"
# env:METRIC_SOURCE_BASIC_AUTH_PASSWORD: metric source basic auth password
METRIC_SOURCE_BASIC_AUTH_PASSWORD="${METRIC_SOURCE_BASIC_AUTH_PASSWORD:-""}"

# env:PUSH_INTERVAL_IN_SECONDS: push interval, if postive value set, push with this interval
PUSH_INTERVAL_IN_SECONDS="${PUSH_INTERVAL_IN_SECONDS:-3}"

# env:CURL: curl command
CURL="${CURL:-$(command -v curl)}"

# arguments: var_name
function require_var {
    local -r var_name="${1-:""}"
    if [[ -z "$var_name" ]]; then
        echo "var_name is required"
        exit 1
    fi
    if [[ -z "${!var_name}" ]]; then
        echo "${var_name} is empty"
        exit 1
    fi
}

source_args=()

require_var "METRIC_SOURCE_URL"
source_args+=("${METRIC_SOURCE_URL}")

if [[ -n "${METRIC_SOURCE_HOST}" ]]; then
    source_args+=("--header")
    source_args+=("Host: ${METRIC_SOURCE_HOST}")
fi
if [[ -n "${METRIC_SOURCE_BASIC_AUTH_USERNAME}" ]] && [[ -n "${METRIC_SOURCE_BASIC_AUTH_PASSWORD}" ]]; then
    source_args+=("--user")
    source_args+=("${METRIC_SOURCE_BASIC_AUTH_USERNAME}:${METRIC_SOURCE_BASIC_AUTH_PASSWORD}")
fi

dest_args=()

require_var "PUSHGATEWAY_METRICS_URL"
dest_args+=("${PUSHGATEWAY_METRICS_URL}")
require_var "PUSHGATEWAY_METHOD"
dest_args+=("--request")
dest_args+=("${PUSHGATEWAY_METHOD}")

if [[ -n "${PUSHGATEWAY_HOST}" ]]; then
    dest_args+=("--header")
    dest_args+=("Host: ${PUSHGATEWAY_HOST}")
fi
if [[ -n "${PUSHGATEWAY_BASIC_AUTH_USERNAME}" ]] && [[ -n "${PUSHGATEWAY_BASIC_AUTH_PASSWORD}" ]]; then
    dest_args+=("--user")
    dest_args+=("${PUSHGATEWAY_BASIC_AUTH_USERNAME}:${PUSHGATEWAY_BASIC_AUTH_PASSWORD}")
fi

function __push() {
    "$CURL" -s "${source_args[@]}" | "$CURL" --data-binary @- "${dest_args[@]}"
}

__push
while [[ "$PUSH_INTERVAL_IN_SECONDS" -gt 0 ]]; do
    sleep "$PUSH_INTERVAL_IN_SECONDS"
    __push
done
# sample_metric.txt:
# # TYPE hello counter
# hello{label="foobar"} 42
# Usage:
# export PUSHGATEWAY_METRICS_URL=http://localhost:9090/metrics/job/foo/instance/bar
# export METRIC_SOURCE_URL=http://localhost:8000/metrics
# export PUSH_INTERVAL_IN_SECONDS=3
# ./pushgateway_curl_run.sh

