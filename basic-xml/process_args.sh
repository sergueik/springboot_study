#!/bin/sh
# Like to perform own argument processing, better than
# java "$@" -jar ${target_jar_env}
#  based on https://stackoverflow.com/questions/3755772/how-to-preserve-double-quotes-in-in-a-shell-script

# TODO: fix the quote insertion to make eval unnecessary
if [ -z $DEBUG ] ; then
  DEBUG=false
fi
for arg in "${@}" ; do
  # figure out if quoting is required for the next argument
  echo $arg | grep -qE '\s'
  if [ $? -eq 0 ] ; then
    if [ $DEBUG = 'true' ] ; then
      echo "the argument needed quotes: \"${arg}\""
      echo $arg | grep -E '\s'
    fi
    # use double quotes around java property setting
    arg=$(echo $arg|sed 's|\(-D[a-z0-9_][a-z0-9_]*\)=\(.*\)$|\1=\"\2\"|')
   fi
 args=$args' '$arg
done
echo "java $args -jar ${target_jar_env}"
# workaround: can not run the command directly
# java $args -jar ${target_jar_env} -jar ${target_jar_env}
# the whitespace-containing arguments get broken apart
eval "java ${args} -jar ${target_jar_env}"

