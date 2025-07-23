#!/bin/sh

# Start JSON exporter
json_exporter --config.file=/json_exporter_config.yml --web.listen-address=:7979 &

