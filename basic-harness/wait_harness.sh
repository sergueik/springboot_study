#!/bin/sh
#
# Harness self-health check
# - tolerates hypervisor instability
# - ignores delegate exit
# - bounded wait
#

PROJECT_FILTER="basic-harness-"
IGNORE_NAME_REGEX="delegate"
SLEEP=5
TIMEOUT=1800

elapsed=0

echo "Running Harness self-health check..."

while :; do
  ids=$(docker ps -aq --filter "name=$PROJECT_FILTER")
  inspect=$(docker inspect $ids 2>/dev/null)

  exited=$(printf '%s\n' "$inspect" |
    jq '[.[] |
      select(.State.Status=="exited") |
      select(.Name | test("'"$IGNORE_NAME_REGEX"'") | not)
    ] | length')

  unhealthy=$(printf '%s\n' "$inspect" |
    jq '[.[] |
      select(.State.Health.Status=="unhealthy")
    ] | length')

  starting=$(printf '%s\n' "$inspect" |
    jq '[.[] |
      select(.State.Health.Status=="starting")
    ] | length')

  if [ "$exited" -gt 0 ]; then
    echo "❌ ERROR: non-delegate container exited"
    printf '%s\n' "$inspect" |
      jq -r '.[] |
        select(.State.Status=="exited") |
        select(.Name | test("'"$IGNORE_NAME_REGEX"'") | not) |
        "\(.Name): exited (\(.State.ExitCode))"'
    exit 1
  fi

  if [ "$unhealthy" -gt 0 ]; then
    echo "❌ ERROR: unhealthy container detected"
    printf '%s\n' "$inspect" |
      jq -r '.[] |
        select(.State.Health.Status=="unhealthy") |
        "\(.Name): unhealthy"'
    exit 1
  fi

  if [ "$starting" -eq 0 ]; then
    echo "✅ Cluster reached acceptable steady state"
    exit 0
  fi

  elapsed=$((elapsed + SLEEP))
  if [ "$elapsed" -ge "$TIMEOUT" ]; then
    echo "⚠️ TIMEOUT: proceeding with $starting container(s) still starting"
    printf '%s\n' "$inspect" |
      jq -r '.[] |
        select(.State.Health.Status=="starting") |
        "\(.Name): still starting"'
    exit 0
  fi

  echo "⏳ Waiting: starting=$starting unhealthy=$unhealthy exited=$exited (${elapsed}s)"
  sleep "$SLEEP"
done
