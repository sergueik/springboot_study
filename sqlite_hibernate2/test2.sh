#!/bin/sh
HOST=192.168.0.25
MAX_COUNT=1000
for cnt in $(seq 1 1 $MAX_COUNT); do curl -s http://$HOST:8080/springboot/getUser?id=10 > /dev/null; done
# MAX_COUNT=10
# for cnt in $(seq 1 1 $MAX_COUNT); do curl -s http://$HOST:8080/springboot/getUser?id=10 ; done
