#!/bin/sh
HOST=192.168.0.25
MAX_COUNT=1000
for cnt in $(seq 1 1 $MAX_COUNT); do curl -s http://$HOST:8080/springboot/getCachedUser?id=10 > /dev/null; done
