#!/usr/bin/sh
echo 'output message 1'
1>&2 echo 'error message 1'
echo 'output message 2'
1>&2 echo 'error message 2'
exit 0
