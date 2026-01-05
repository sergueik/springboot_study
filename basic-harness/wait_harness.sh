#!/bin/sh

PROJECT_FILTER="basic-harness-"
IGNORE_FOR_GATE_REGEX="delegate"

SLEEP=5
TIMEOUT=1800

INSPECT_MODE=0

# parse flags
for arg in "$@"; do
  case "$arg" in
    -i|--inspect)
      INSPECT_MODE=1
      ;;
  esac
done

elapsed=0

echo "Starting Harness self-health check"

while :; do
  ids=$(docker ps -aq --filter "name=$PROJECT_FILTER")
  [ -z "$ids" ] && {
    echo "No matching containers found"
    exit 1
  }

  inspect_json=$(docker inspect $ids 2>/dev/null)

  # Stage 1: unwind array
  containers=$(printf '%s\n' "$inspect_json" | jq '.[]')

  # Stage 2: factual classification (NO policy)
  exited_all=$(printf '%s\n' "$containers" | jq -r '
    select(.State.Status=="exited") |
    (.Name + ": exited (code=" + (.State.ExitCode|tostring) + ")")
  ')

  unhealthy_all=$(printf '%s\n' "$containers" | jq -r '
    select(.State.Health.Status=="unhealthy") |
    (.Name + ": unhealthy")
  ')

  starting_all=$(printf '%s\n' "$containers" | jq -r '
    select(.State.Health.Status=="starting") |
    (.Name + ": starting")
  ')

  # Policy for gating (domain-specific)
  exited_gate=$(printf '%s\n' "$containers" | jq -r '
    select(.State.Status=="exited") |
    select(.Name | test("'"$IGNORE_FOR_GATE_REGEX"'") | not) |
    (.Name + ": exited (code=" + (.State.ExitCode|tostring) + ")")
  ')

  unhealthy_gate="$unhealthy_all"

  exited_gate_count=$(printf '%s\n' "$exited_gate" | sed '/^$/d' | wc -l)
  unhealthy_gate_count=$(printf '%s\n' "$unhealthy_gate" | sed '/^$/d' | wc -l)
  starting_count=$(printf '%s\n' "$starting_all" | sed '/^$/d' | wc -l)

  # FAILURE decision
  if [ "$exited_gate_count" -gt 0 ] || [ "$unhealthy_gate_count" -gt 0 ]; then
    echo "FAILURE detected"

    if [ "$INSPECT_MODE" -eq 1 ]; then
      if [ -n "$exited_all" ]; then
        echo ""
        echo "Exited containers:"
        printf '%s\n' "$exited_all"
      fi

      if [ -n "$unhealthy_all" ]; then
        echo ""
        echo "Unhealthy containers:"
        printf '%s\n' "$unhealthy_all"
      fi
    else
      if [ "$exited_gate_count" -gt 0 ]; then
        echo "$exited_gate" | sed -n '1p'
      elif [ "$unhealthy_gate_count" -gt 0 ]; then
        echo "$unhealthy_gate" | sed -n '1p'
      fi
    fi

    exit 1
  fi

  # SUCCESS condition
  if [ "$starting_count" -eq 0 ]; then
    echo "All containers reached acceptable steady state"
    exit 0
  fi

  elapsed=$((elapsed + SLEEP))
  if [ "$elapsed" -ge "$TIMEOUT" ]; then
    echo "Timeout reached"

    if [ "$INSPECT_MODE" -eq 1 ] && [ -n "$starting_all" ]; then
      echo ""
      echo "Still starting:"
      printf '%s\n' "$starting_all"
    fi

    exit 0
  fi

  echo "Waiting: starting=$starting_count elapsed=${elapsed}s"
  sleep "$SLEEP"
done
