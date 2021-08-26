#!/bin/sh
# NOTE: line endings-sensitive
set -e

if [ ! -z "$GF_INSTALL_PLUGINS" ]; then
  OLDIFS=$IFS
  IFS=','
  echo "Installing $GF_INSTALL_PLUGINS"
  for PLUGIN in $GF_INSTALL_PLUGINS
  do
    grafana-cli plugins install $PLUGIN
  done
  IFS=$OLDIFS
fi

chown -R grafana:grafana /grafana
# exec su-exec grafana /usr/bin/grafana-server --homepath=/grafana restart
exec su-exec grafana /usr/bin/grafana-server --homepath=/grafana
