#!/bin/sh 
# bad substitution in regular shell
# works fine with bash and ash (alpine)
MSG='port 123'
if [[ "${MSG//[^a-z ]/}" == 'port ' ]]; then
  echo "PORT = ${MSG//[^0-9]/}"; 
else
  echo 'Failed to use bash syntax'
fi
MSG='host mysql_db'
if [[ "${MSG// [a-z_]*/}" == 'host' ]]; then
  echo "HOST = ${MSG//host /}"; 
else
  echo 'Failed to use bash syntax extensions'
fi

