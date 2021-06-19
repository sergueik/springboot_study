#!/bin/sh
set -e

if [ ! -z ${GF_INSTALL_PLUGINS} ]; then
  OLDIFS=$IFS
  IFS=','
  for plugin in ${GF_INSTALL_PLUGINS}; do
    grafana-cli plugins install ${plugin}
  done
  IFS=$OLDIFS
fi

chown -R grafana:grafana /grafana
# exec su-exec grafana /usr/bin/grafana-server --homepath=/grafana restart
exec su-exec grafana /usr/bin/grafana-server --homepath=/grafana
