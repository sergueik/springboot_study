# origin: https://github.com/OSSHelp/pushgateway-functions/blob/master/pushgateway-functions/pushgateway-functions.sh
# shellcheck disable=SC2206,SC2034,SC2148,SC2207

umask 0077
export LANG=C
export LC_ALL=C

## default vars
pfver=1.1.0
pf_debug=0
myhostname=$(hostname -f)
curl=$(command -v curl)

## Pushgateway vars
pushgateway_host="netdata-master"
pushgateway_port=9091
pushgateway_url_path="/metrics/job/${0##*/}/source/${myhostname%.*}"
pushgateway_url="http://${pushgateway_host}:${pushgateway_port}${pushgateway_url_path}"
pushgateway_opts=()
metrics_tmp_file=$(mktemp /tmp/pf-$$-XXXXXXXXXX)

declare -A metric_name
declare -A metric_type
declare -A metric_help
declare -A metric_value
declare -A metric_labels

## Helpers
function file_readable_nonempty() { test -r "${1}" -a -s "${1}"; }
function show_error() {
    local message="${1}"; local funcname="${2}"; log_date=$(date '+%Y/%m/%d:%H:%M:%S')
    echo -e "[ERROR.${funcname} ${log_date}] ${message}" >&2
}
function show_notice() {
    local message="${1}"; local funcname="${2}"; log_date=$(date '+%Y/%m/%d:%H:%M:%S')
    echo -e "[NOTICE.${funcname} ${log_date}] ${message}"
}
function show_debug() {
    local message="${1}"; local funcname="${2}"; log_date=$(date '+%Y/%m/%d:%H:%M:%S')
    echo -e "[DEBUG.${funcname} ${log_date}] ${message}"
}

## Validation functions
# Details: https://prometheus.io/docs/practices/naming/
function metric_name_is_valid() {
  local name="${1}"
  local err=0
  test "${#name}" -gt 0 || \
    err=1
  [[ "${name}" =~ [a-zA-Z_:][a-zA-Z0-9_:]* ]] || \
    err=1
  return "${err}"
}

# Details: https://prometheus.io/docs/concepts/metric_types/
function metric_type_is_valid() {
  local type="${1}"
  grep -qE '^(gauge|counter|histogram|summary)$' <<< "${type}"
}

# Check if value is a decimal number
function metric_value_is_valid() {
  local value="${1}"
  num=$(printf '%s' "${value}" | sed -e "s/^0*\([1-9]\)/\1/" -e "s/'/^/")
  test "${num}" && printf '%f' "${num}" >/dev/null 2>&1
}

# Details: https://prometheus.io/docs/practices/naming/
function metric_label_names_are_valid() {
  local err=0
  local labels="${*}"
  local label_names=($(sed -r 's/=\w+//g' <<< "${labels}"))

  for name in "${label_names[@]}"; do
    [[ "${name}" =~ [a-zA-Z_][a-zA-Z0-9_]* ]] || \
      err=1
  done

  return "${err}"
}

## Main functions
function pushgateway_register_metric() {
  metric_name[$1]="${1}"
  metric_type[$1]="${2}"
  metric_help[$1]="${3}"
}
function pushgateway_set_value() {
  local err=0
  local validation_success=0
  local help_presence=0
  local metric_value[$1]=${2}
  local metric_raw_labels[$1]="${*:3}"

  # Validation
  metric_name_is_valid "${metric_name[$1]}" && \
    metric_type_is_valid "${metric_type[$1]}" && \
      metric_value_is_valid "${metric_value[$1]}" && \
        metric_label_names_are_valid "${metric_raw_labels[$1]}" && \
          validation_success=1

  test "${validation_success}" -eq 1 || {
    show_error "Validation failed: check provided metric name, type, value and labels correctness."
    err=1
  }

  test "${validation_success}" -eq 1 && {
    # Formatting given labels from 'label1=value1 label2=value2' to 'label1="value1",label2="value2"'
    metric_formatted_labels="{$(sed -r 's/(\w+)(=)(\S+)/\1="\3"/g;s/\s/,/g' <<< "${metric_raw_labels[*]}")}"
    test "${metric_raw_labels[*]}" == "none" -o -z "${metric_raw_labels[*]}" && \
      metric_formatted_labels=''

    test -f "${metrics_tmp_file}" && \
      grep -q "^\# HELP ${metric_name[$1]}" "${metrics_tmp_file}" && \
        help_presence=1

    test "${help_presence}" -eq 1 && \
      sed -ri "/^#\sTYPE\s${metric_name[$1]}\s.*/a ${metric_name[$1]}${metric_formatted_labels} ${metric_value[$1]}" "${metrics_tmp_file}"

    test "${help_presence}" -eq 0 && {
cat <<EOF >> "${metrics_tmp_file}"
# HELP ${metric_name[$1]} ${metric_help[$1]}
# TYPE ${metric_name[$1]} ${metric_type[$1]}
${metric_name[$1]}${metric_formatted_labels} ${metric_value[$1]}
EOF
}

    file_readable_nonempty "${metrics_tmp_file}" || {
      err=1
      show_error "Error on adding metric to temp file (${metrics_tmp_file}). Empty or not readable." "${FUNCNAME[0]}"
    }
  }
  return "${err}"
}

function pushgateway_send_metrics() {
  local err=1
  local grouping_keys=(${@})
  local grouping_keys_formatted

  # Formatting given keys from 'key1=value1 key2=value2' to '/key1/value1/key2/value2'
  grouping_keys_formatted=$(sed -r 's/(\w+)(=)(\S+)(\s)?/\/\1\/\3/g' <<< "${grouping_keys[*]}")

  test -x "${curl}" || {
    show_error "Can't find curl binary!" "${FUNCNAME[0]}"
    return "${err}"
  }

  http_code=$(curl -sq "${pushgateway_opts[@]}" --data-binary "@${metrics_tmp_file}" -o /dev/null -w "%{http_code}" "${pushgateway_url}${grouping_keys_formatted}")
  test "${http_code}" == "200" && \
    err=0

  test "${pf_debug}" -gt 0 && {
    show_debug "Prepared metrics\n$(cat "${metrics_tmp_file}")"
    show_debug "Pushgateway http code was ${http_code}"
    show_debug "Error code is ${err}"
  }

  test "${err}" -ne 0 && \
    show_error "Error on sending metrics to Pushgateway. Check url and params provided." "${FUNCNAME[0]}"
  test "${err}" -eq 0 && \
    show_notice "Metrics were successfully sent to Pushgateway." "${FUNCNAME[0]}"

  rm "${metrics_tmp_file}"

  return "${err}"
}
