#!/bin/sh
1>&2 echo 'error message'
echo 'console message'
EXITCODE=42
exit $EXITCODE
