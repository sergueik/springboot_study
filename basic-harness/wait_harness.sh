#!/bin/sh

PROJECT_FILTER="basic-harness-"
IGNORE_NAME_REGEX="delegate"
SLEEP=5
TIMEOUT=1800

INSPECT_MODE=0

# flag parsing
for arg in "$@"; do
  case "$arg" in
    -i|--inspect)
      INSPECT_MODE=1
      ;;
  esac
done

elapsed=0

echo "Running Harness self-health check..."

while :; do
  ids=$(docker ps -aq --filter "name=$PROJECT_FILTER")
  inspect=$(docker inspect $ids 2>/dev/null)

  exited_info=$(printf '%s\n' "$inspect" | jq -r '
    .[] |
    select(.State.Status=="exited") |
    select(.Name | test("'"$IGNORE_NAME_REGEX"'") | not) |
    "\(.Name): exited (code=\(.State.ExitCode))"
  ')

  unhealthy_info=$(printf '%s\n' "$inspect" | jq -r '
    .[] |
    select(.State.Health.Status=="unhealthy") |
    "\(.Name): unhealthy"
  ')

  starting_info=$(printf '%s\n' "$inspect" | jq -r '
    .[] |
    select(.State.Health.Status=="starting") |
    "\(.Name): starting"
  ')

  exited_count=$(printf '%s\n' "$exited_info" | sed '/^$/d' | wc -l)
  unhealthy_count=$(printf '%s\n' "$unhealthy_info" | sed '/^$/d' | wc -l)
  starting_count=$(printf '%s\n' "$starting_info" | sed '/^$/d' | wc -l)

  # FAILURES detected
  if [ "$exited_count" -gt 0 ] || [ "$unhealthy_count" -gt 0 ]; then
    echo "❌ FAILURE detected"

    if [ "$INSPECT_MODE" -eq 1 ]; then
      [ "$exited_count" -gt 0 ] && {
        echo ""
        echo "Exited containers:"
        printf '%s\n' "$exited_info"
      }

      [ "$unhealthy_count" -gt 0 ] && {
        echo ""
        echo "Unhealthy containers:"
        printf '%s\n' "$unhealthy_info"
      }
    else
      # fail fast
      [ "$exited_count" -gt 0 ] && {
        echo "$exited_info" | head -n 1
      }
      [ "$unhealthy_count" -gt 0 ] && {
        echo "$unhealthy_info" | head -n 1
      }
    fi

    exit 1
  fi

  # SUCCESS condition
  if [ "$starting_count" -eq 0 ]; then
    echo "✅ Cluster reached acceptable steady state"
    exit 0
  fi

  elapsed=$((elapsed + SLEEP))
  if [ "$elapsed" -ge "$TIMEOUT" ]; then
    echo "⚠️ TIMEOUT reached"

    if [ "$INSPECT_MODE" -eq 1 ]; then
      echo ""
      echo "Still starting:"
      printf '%s\n' "$starting_info"
    fi

    exit 0
  fi

  echo "⏳ Waiting: starting=$starting_count (${elapsed}s)"
  sleep "$SLEEP"
done
