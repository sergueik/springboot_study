#!/bin/sh 
DATA=$(cat -)
# NOTE: on Alpine jq is in /usr/bin
JQ='/usr/bin/jq'
if [ ! -z "$OS" ] ; then
  # On Windows assume running from git bash shell and use custom path to the tool
  JQ=/c/tools/jq-win64.exe
fi
echo $DATA | $JQ '.'
