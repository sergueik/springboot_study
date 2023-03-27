#!/bin/sh
# based on https://www.unix.com/shell-programming-and-scripting/232805-parsing-http-post-request.html
# NOTE: the following does not work
# REQUEST_METHOD=POST CONTEXT_LENGTH=16 echo '{"1234": "5678"}' | ./echo.sh
# need to export:
# export REQUEST_METHOD=POST; export CONTEXT_LENGTH=16; echo '{"1234": "5678"}' | ./echo.sh
# NOTE: on Alpine jq is in /usr/bin
JQ='/usr/bin/jq'
if [ ! -z "$OS" ] ; then
  # On Windows assume running from git bash shell and use custom path to the tool
  JQ=/c/tools/jq-win64.exe
fi

if  [ "$REQUEST_METHOD" = 'POST' ] ; then
  if  [ "$CONTEXT_LENGTH" -gt 0 ]; then
    while read -n $CONTEXT_LENGTH DATA <&0
    do
      echo $DATA | $JQ '.'
    done
  fi
fi
