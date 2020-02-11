#!/bin/bash

# Like to perform own argument processing, better than
# java "$@" -jar ${target_jar_env}
#  based on https://stackoverflow.com/questions/3755772/how-to-preserve-double-quotes-in-in-a-shell-script

# TODO: fix the quote insertion to make eval unnecessary
if [ -z $DEBUG ] ; then
  DEBUG=false
fi

declare -a _args
_args[0]=''
for arg in "${@}" ; do
  # figure out if quoting is required for the next argument
  _argc=${#_args[*]}
  _argc=$(expr $_argc + 1)
  if [ $? -eq 0 ] ; then
    if [ $DEBUG = 'true' ] ; then
      echo "the argument needed quotes: [${arg}]"
    fi
    # use double quotes around java property setting
    arg=$(echo $arg|sed 's/\(-D[a-z0-9_][a-z0-9_]*\)=\(.*\)$/\1="\2"/')
    if [ $DEBUG = 'true' ] ; then
      echo "the argument becomes: [${arg}]"
    fi
  fi
  _args[$_argc]=$arg
done
echo "java ${_args[@]} -jar ${target_jar_env}"
# the whitespace-containing arguments still get broken apart
if [ $DEBUG = 'true' ] ; then
  exit 0
fi

eval "java ${_args[@]} -jar ${target_jar_env}"
