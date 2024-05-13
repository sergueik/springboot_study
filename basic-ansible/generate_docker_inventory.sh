#!/bin/bash
CONTAINER=${1}
if [[ -z "$CONTAINER" ]] ; then
  echo 'container name cannot be blank'
  exit 0
fi
if [ "$DEBUG" = 'true' ] ; then
  echo  "check if container ${CONTAINER} exists and has is running"
fi

if [ "$DEBUG" = 'true' ] ; then
  echo "docker container inspect $CONTAINER -f '{{ .State.Status }}'"
fi
STATUS=$(docker container inspect $CONTAINER -f '{{ .State.Status }}')
if [ "${STATUS}" != 'running' ]; then
  echo "Cannot determine IP address of a non running container $CONTAINER"
  exit 0

fi
if [ "$DEBUG" = 'true' ] ; then
  echo "STATUS=${STATUS}"
fi
NETWORK=${2:-default}
if [ "$DEBUG" = 'true' ] ; then
  echo "NETWORK=${NETWORK}"
fi
if [ "${NETWORK}" != 'default' ]; then
  
  if [ "$DEBUG" = 'true' ] ; then
    echo "check if network $NETWORK exists"
  fi
  docker network ls |grep -q "$NETWORK"
  if [[ $? -ne 0 ]] ; then
    echo "invalid network name: $NETWORK"
    exit 0
  fi
  if [ "$DEBUG" = 'true' ] ; then
    echo  "check if container ${CONTAINER} is attached to network $NETWORK"
  fi
  if [ "$DEBUG" = 'true' ] ; then
    echo  "docker container inspect $CONTAINER -f '{{ .HostConfig.NetworkMode }}'"
  fi
  NETWORK_CONFIG=$(docker container inspect $CONTAINER -f '{{ .HostConfig.NetworkMode }}')

  if [ "$DEBUG" = 'true' ] ; then
    echo "The container $CONTAINER configured with network $NETWORK_CONFIG"
  fi


  if [ "${NETWORK}" != "$NETWORK_CONFIG" ]; then
    echo "invalid network argument $NETWORK for container $CONTAINER, aborting"
    exit 0
  fi
# NOTE: for 'bridge' will be 'default' - not testing

else
  if [ "$DEBUG" = 'true' ] ; then
    echo "setting $NETWORK to 'bridge'"
  fi
  NETWORK='bridge'
fi
if [ "$DEBUG" = 'true' ] ; then
  echo "docker container inspect -f \"{{ .NetworkSettings.Networks.${NETWORK}.IPAddress}}\" $CONTAINER"
fi

# NOTE: by default the docker-compose creates  networks with '-' in the name:
# 'basicelkcluster2_aspnetcore-app-seq-elastic'
# this leads to the problem:
# IP_ADDRESS=$(docker container inspect -f "{{ .NetworkSettings.Networks.${NETWORK}.IPAddress}}" $CONTAINER)
# template parsing error: template: :1: bad character U+002D '-'
# enclosing in double quotes does not help:
# IP_ADDRESS=$(docker container inspect -f "{{ .NetworkSettings.Networks.\"${NETWORK}\".IPAddress}}" $CONTAINER)
# template parsing error: template: :1: bad character U+0022 '"'
# Workaround is in query expression change:
# origin: https://github.com/moby/moby/issues/35886
if [ "$DEBUG" = 'true' ] ; then
   echo "docker container inspect -f \"{{ \\\$network := index .NetworkSettings.Networks \\\"$NETWORK\\\" }}{{ \$network.IPAddress}}\" $CONTAINER"
fi
IP_ADDRESS=$(docker container inspect -f "{{ \$network := index .NetworkSettings.Networks \"$NETWORK\" }}{{ \$network.IPAddress}}" $CONTAINER)
if [ "$DEBUG" = 'true' ] ; then
  echo "IP_ADDRESS=${IP_ADDRESS}"
fi
if [ "$DEBUG" = 'true' ] ; then
  echo "docker container inspect $CONTAINER| jq -cr \".[].NetworkSettings.Networks.\\\"$NETWORK\\\".IPAddress\""
fi

IP_ADDRESS=$(docker container inspect $CONTAINER| jq -cr ".[].NetworkSettings.Networks.\"$NETWORK\".IPAddress")
if [ "$DEBUG" = 'true' ] ; then
  echo "IP_ADDRESS=${IP_ADDRESS}"
fi
echo "$IP_ADDRESS ansible_host=$CONTAINER connection=docker"

