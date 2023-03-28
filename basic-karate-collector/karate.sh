#!/bin/sh
WORKDIR=$(dirname $0)
java -cp "${WORKDIR}/karate.jar":. com.intuit.karate.Main "$@"
