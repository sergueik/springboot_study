#! /usr/bin/env bash

set -eo pipefail

CONFIG='/opt/zookeeper/conf/zoo.cfg'
ZOOKEEPER_ID=${ZOOKEEPER_ID:-1}
echo "ZOOKEEPER_ID=$ZOOKEEPER_ID"

echo $ZOOKEEPER_ID > /var/lib/zookeeper/myid

ZOOKEEPER_TICK_TIME=${ZOOKEEPER_TICK_TIME:-2000}
echo "tickTime=${ZOOKEEPER_TICK_TIME}" | tee $CONFIG 

ZOOKEEPER_INIT_LIMIT=${ZOOKEEPER_INIT_LIMIT:-10}
echo "initLimit=${ZOOKEEPER_INIT_LIMIT}" | tee -a $CONFIG

ZOOKEEPER_SYNC_LIMIT=${ZOOKEEPER_SYNC_LIMIT:-5}
echo "syncLimit=${ZOOKEEPER_SYNC_LIMIT}" | tee -a $CONFIG

echo "dataDir=/var/lib/zookeeper" | tee -a $CONFIG
echo "clientPort=2181" | tee -a $CONFIG

ZOOKEEPER_CLIENT_CNXNS=${ZOOKEEPER_CLIENT_CNXNS:-60}
echo "maxClientCnxns=${ZOOKEEPER_CLIENT_CNXNS}" | tee -a $CONFIG

ZOOKEEPER_AUTOPURGE_SNAP_RETAIN_COUNT=${ZOOKEEPER_AUTOPURGE_SNAP_RETAIN_COUNT:-3}
echo "autopurge.snapRetainCount=${ZOOKEEPER_AUTOPURGE_SNAP_RETAIN_COUNT}" | tee -a $CONFIG 

ZOOKEEPER_AUTOPURGE_PURGE_INTERVAL=${ZOOKEEPER_AUTOPURGE_PURGE_INTERVAL:-0}
echo "autopurge.purgeInterval=${ZOOKEEPER_AUTOPURGE_PURGE_INTERVAL}"| tee -a $CONFIG 

for VAR in $(env); do
  if [[ $VAR =~ ^ZOOKEEPER_SERVER_[0-9]+= ]]; then
    SERVER_ID=`echo "$VAR" | sed -r 's/ZOOKEEPER_SERVER_(.*)=.*/\1/'`
    SERVER_IP=`echo "$VAR" | sed 's/.*=//'`
    if [ "${SERVER_ID}" = "${ZOOKEEPER_ID}" ]; then
      echo "server.${SERVER_ID}=0.0.0.0:2888:3888" | tee -a $CONFIG
    else
      echo "server.${SERVER_ID}=${SERVER_IP}:2888:3888" | tee -a $CONFIG
    fi
  fi
done

mkdir /opt/zookeeper/logs                                                       
su zookeeper -s /bin/bash -c "/opt/zookeeper/bin/zkServer.sh print-cmd"     
su zookeeper -s /bin/bash -c "/opt/zookeeper/bin/zkServer.sh start-foreground"
