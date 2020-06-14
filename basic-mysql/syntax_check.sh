#!/bin/bash
# works fine with bash and ash (alpine)
# bad substitution error indicates that the regular shell is processing
# see also: https://www.cyberciti.biz/tips/bash-shell-parameter-substitution-2.html
MSG='port 3360'
if [[ "${MSG//[^a-z ]/}" == 'port ' ]]; then
  PORT=${MSG//[^0-9]/}
else
  echo 'Failed to use bash syntax, using default port'
  PORT=3306
fi
echo "PORT = ${PORT}";

MSG='host mysql_host'
# NOTE: ${MSG// [a-z_]/} would fail
if [[ "${MSG/ [a-z_]*/}" == 'host' ]]; then
  HOST=${MSG//host /}
else
  echo 'Failed to use bash syntax extensions'
  HOST='localhost'
fi

echo "HOST = ${HOST}";

