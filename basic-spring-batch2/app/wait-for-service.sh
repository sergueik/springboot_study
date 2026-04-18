#!/bin/sh
# NOTE: ash ready

SERVICE_HOST="${SERVICE_HOST:-mysql-server}"
SERVICE_PORT="${SERVICE_PORT:-3306}"
DELAY="${DELAY:-5}"
MAX_RETRY="${MAX_RETRY:-30}"

RETRY_CNT=0

echo "Waiting for ${SERVICE_HOST}:${SERVICE_PORT}..."

while true
do
    nc -z "${SERVICE_HOST}" "${SERVICE_PORT}" >/dev/null 2>&1

    if [ $? -eq 0 ]; then
        echo "Service is available"
        break
    fi

    RETRY_CNT=$((RETRY_CNT + 1))

    if [ "${RETRY_CNT}" -ge "${MAX_RETRY}" ]; then
        echo "Timeout waiting for ${SERVICE_HOST}:${SERVICE_PORT}"
        exit 1
    fi

    echo "Retry ${RETRY_CNT}/${MAX_RETRY}..."
    sleep "${DELAY}"
done

echo "Starting: $@"
exec "$@"
