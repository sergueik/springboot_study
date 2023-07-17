#!/bin/sh
# based on: https://github.com/JonnyBGod/docker-packetbeat/blob/master/docker-entrypoint.sh
# see also: https://github.com/docker-flow/docker-flow-proxy/blob/main/Dockerfile.packetbeat

set -e


if [ "$1" = 'start' ]; then
  setConfiguration() {
    KEY=$1
    VALUE=$2
    sed -i "s/{{$KEY}}/$VALUE/g" ${PACKETBEAT_HOME}/packetbeat.yml
  }

  if [ -n "${LOGSTASH_HOST+1}" ]; then
    setConfiguration "LOGSTASH_HOST" "$LOGSTASH_HOST"
  else
    echo "LOGSTASH_HOST is needed"
    exit 1
  fi

  if [ -n "${LOGSTASH_PORT+1}" ]; then
    setConfiguration "LOGSTASH_PORT" "$LOGSTASH_PORT"
  else
    echo "LOGSTASH_PORT is needed"
    exit 1
  fi

  sed -i "s#{{DEVICE}}#${DEVICE:=any}#g" ${PACKETBEAT_HOME}/packetbeat.yml
  sed -i "s#{{INDEX}}#${INDEX:=packetbeat}#g" ${PACKETBEAT_HOME}/packetbeat.yml
  sed -i "s#{{LOG_LEVEL}}#${LOG_LEVEL:=error}#g" ${PACKETBEAT_HOME}/packetbeat.yml
  sed -i "s#{{SHIPPER_NAME}}#${SHIPPER_NAME:=`hostname`}#g" ${PACKETBEAT_HOME}/packetbeat.yml
  sed -i "s#{{SHIPPER_TAGS}}#${SHIPPER_TAGS}#g" ${PACKETBEAT_HOME}/packetbeat.yml

  echo "Initializing Packetbeat..."
  ${PACKETBEAT_HOME}/packetbeat -e -v -c ${PACKETBEAT_HOME}/packetbeat.yml

  wait
else
  exec "$@"
fi
