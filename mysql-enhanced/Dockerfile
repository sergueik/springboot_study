FROM mysql:8.0.18
EXPOSE 3306 33060
CMD ["mysqld", "sh -c \"while true; do netstat -ant | grep -q 3036; if [ $$? -eq 0  ] ; then break ;  fi ;  echo \\\"x\\\"; sleep 10;  done\""]
# One cannot modify the stock image "CMD" or "ENTRYPOINT" - they are already finished commands and assume the added command is an argument:
# [ERROR] [MY-010147] [Server] Too many arguments (first extra is 'sh -c "while true; do netstat -ant | grep -q 3036; if [ $$? -eq 0  ] ; then break ;  fi ;  echo \"x\"; sleep 10;  done"').
# https://github.com/docker-library/mysql/blob/master/8.0/Dockerfile
