### Info

 this repository contains a variation of extra node `docker-compose.yml` for waiting on stateful containers explained in [repo](https://github.com/billylaing/docker-wait-for-it-alpine) but utilizing the stadalone network peer availability [utility](https://github.com/erikogan/wait4ports) available on alpine which is faster to install than bash and copy the bash script
```sh
ADD https://raw.githubusercontent.com/vishnubob/wait-for-it/master/wait-for-it.sh /opt/bin/
```
### Usage

####  Without docker-compose
```sh
docker run --name nginx -d nginx

```
```sh
docker build -t basic-wait4ports -f Dockerfile .
```
```sh
docker  run --name basic-wait4ports --link nginx -e PORT=80 -e NODE=nginx -e
NAME=nginx -e TIMEOUT=10 -it  basic-wait4ports
```
```
PORT READY: nginx
```

#### With docker-compose
##### nginx example
```sh
docker-compose -f docker-compose.nginx.yml up --build
```
* cleanup

```sh
docker-compose -f docker-compose.ngnx.yml stop
docker-compose -f docker-compose.ngnx.yml rm -f
```
##### mysql example
* prepare
```sh
docker pull mysql:8.0.18
```
* start
```sh
docker-compose -f docker-compose.mysql.yml up --build
```
the console log
shows the timeline of the cluster boot:

```text
Building wait
Step 1/3 : FROM alpine:3.9.5
 ---> 82f67be598eb
Step 2/3 : RUN apk add --no-cache wait4ports
 ---> Using cache
 ---> 3db1c688af4b
Step 3/3 : ENTRYPOINT ["sh", "-c", "wait4ports -s $TIMEOUT ${NAME}=tcp://${NODE}:${PORT}"]
 ---> Using cache
 ---> 3afc3d067eaf
Successfully built 3afc3d067eaf
Successfully tagged wait4ports:latest
Creating basic-wait4ports_wait_1 ... done
Creating mysql-server            ... done
Attaching to basic-wait4ports_wait_1, mysql-server
mysql-server    | 2022-12-02 18:48:31+00:00 [Note] [Entrypoint]: Entrypoint script for MySQL Server 8.0.18-1debian9 started.
mysql-server    | 2022-12-02 18:48:31+00:00 [Note] [Entrypoint]: Switching to dedicated user 'mysql'
mysql-server    | 2022-12-02 18:48:31+00:00 [Note] [Entrypoint]: Entrypoint script for MySQL Server 8.0.18-1debian9 started.
mysql-server    | 2022-12-02 18:48:31+00:00 [Note] [Entrypoint]: Initializing database files
mysql-server    | 2022-12-02T18:48:31.803596Z 0 [Warning] [MY-011070] [Server] 'Disabling symbolic links using --skip-symbolic-links (or equivalent) is the default. Consider not using this option as it' is deprecated and will be removed ina future release.
mysql-server    | 2022-12-02T18:48:31.804043Z 0 [System] [MY-013169] [Server] /usr/sbin/mysqld (mysqld 8.0.18) initializing of server in progress as process 43
wait_1          | Trying  (172.19.0.3:3306) ... failed.
wait_1          | Trying  (172.19.0.3:3306) ... failed.
mysql-server    | 2022-12-02T18:48:40.985885Z 5 [Warning] [MY-010453] [Server] rot@localhost is created with an empty password ! Please consider switching off the --initialize-insecure option.
mysql-server    | 2022-12-02 18:48:59+00:00 [Note] [Entrypoint]: Database files initialized
mysql-server    | 2022-12-02 18:48:59+00:00 [Note] [Entrypoint]: Starting temporary server
mysql-server    | 2022-12-02T18:48:59.981028Z 0 [Warning] [MY-011070] [Server] 'Disabling symbolic links using --skip-symbolic-links (or equivalent) is the defa
ult. Consider not using this option as it' is deprecated and will be removed in a future release.
wait_1          | Trying  (172.19.0.3:3306) ... failed.
mysql-server    | 2022-12-02T18:48:59.981485Z 0 [System] [MY-010116] [Server] /usr/sbin/mysqld (mysqld 8.0.18) starting as process 92
mysql-server    | 2022-12-02T18:49:02.114402Z 0 [Warning] [MY-010068] [Server] CA certificate ca.pem is self signed.
mysql-server    | 2022-12-02T18:49:02.125659Z 0 [Warning] [MY-011810] [Server] Insecure configuration for --pid-file: Location '/var/run/mysqld' in the path is accessible to all OS users. Consider choosing a different directory.
mysql-server    | 2022-12-02T18:49:02.214837Z 0 [System] [MY-010931] [Server] /usr/sbin/mysqld: ready for connections. Version: '8.0.18'  socket: '/var/run/mysqld/mysqld.sock'  port: 0  MySQL Community Server - GPL.
mysql-server    | 2022-12-02T18:49:02.247857Z 0 [System] [MY-011323] [Server] XPlugin ready for connections. Socket: '/var/run/mysqld/mysqlx.sock'
wait_1          | Trying  (172.19.0.3:3306) ... failed.
mysql-server    | 2022-12-02 18:49:02+00:00 [Note] [Entrypoint]: Temporary server started.
mysql-server    | Warning: Unable to load '/usr/share/zoneinfo/iso3166.tab' as time zone. Skipping it.
wait_1          | Trying  (172.19.0.3:3306) ... failed.
wait_1          | Trying  (172.19.0.3:3306) ... failed.
wait_1          | Trying  (172.19.0.3:3306) ... failed.
mysql-server    | Warning: Unable to load '/usr/share/zoneinfo/leap-seconds.list' as time zone. Skipping it.
mysql-server    | Warning: Unable to load '/usr/share/zoneinfo/zone.tab' as time zone. Skipping it.
mysql-server    | Warning: Unable to load '/usr/share/zoneinfo/zone1970.tab' as time zone. Skipping it.
mysql-server    | 2022-12-02 18:49:47+00:00 [Note] [Entrypoint]: Creating database test
mysql-server    | 2022-12-02 18:49:48+00:00 [Note] [Entrypoint]: Creating user java
mysql-server    | 2022-12-02 18:49:48+00:00 [Note] [Entrypoint]: Giving user java access to schema test
mysql-server    |
mysql-server    | 2022-12-02 18:49:48+00:00 [Note] [Entrypoint]: Stopping temporary server
mysql-server    | 2022-12-02T18:49:48.219758Z 14 [System] [MY-013172] [Server] Received SHUTDOWN from user root. Shutting down mysqld (Version: 8.0.18).
mysql-server    | 2022-12-02T18:49:49.872324Z 0 [System] [MY-010910] [Server] /usr/sbin/mysqld: Shutdown complete (mysqld 8.0.18)  MySQL Community Server - GPL.
mysql-server    | 2022-12-02 18:49:50+00:00 [Note] [Entrypoint]: Temporary server stopped
mysql-server    |
mysql-server    | 2022-12-02 18:49:50+00:00 [Note] [Entrypoint]: MySQL init process done. Ready for start up.
mysql-server    |
mysql-server    | 2022-12-02T18:49:50.938240Z 0 [Warning] [MY-011070] [Server] 'Disabling symbolic links using --skip-symbolic-links (or equivalent) is the default. Consider not using this option as it' is deprecated and will be removed in a future release.
wait_1          | Trying  (172.19.0.3:3306) ... failed.
mysql-server    | 2022-12-02T18:49:50.938557Z 0 [System] [MY-010116] [Server] /usr/sbin/mysqld (mysqld 8.0.18) starting as process 1
mysql-server    | 2022-12-02T18:49:52.596729Z 0 [Warning] [MY-010068] [Server] CA certificate ca.pem is self signed.
mysql-server    | 2022-12-02T18:49:52.613340Z 0 [Warning] [MY-011810] [Server] Insecure configuration for --pid-file: Location '/var/run/mysqld' in the path is accessible to all OS users. Consider choosing a different directory.
mysql-server    | 2022-12-02T18:49:52.720670Z 0 [System] [MY-010931] [Server] /usr/sbin/mysqld: ready for connections. Version: '8.0.18'  socket: '/var/run/mysqld/mysqld.sock'  port: 3306  MySQL Community Server - GPL.
wait_1          | Trying  (172.19.0.3:3306) ... failed.
wait_1          | Trying  (172.19.0.3:3306) ... SUCCESS!

```
if the `-q` option is provided there is little logs from `wait` node, only the below
```text
mysql-server    | 2022-12-02T17:40:51.833311Z 0 [Warning] [MY-011810] [Server] I
nsecure configuration for --pid-file: Location '/var/run/mysqld' in the path is
accessible to all OS users. Consider choosing a different directory.
mysql-server    | 2022-12-02T17:40:51.926170Z 0 [System] [MY-010931] [Server] /u
sr/sbin/mysqld: ready for connections. Version: '8.0.18'  socket: '/var/run/mysq
ld/mysqld.sock'  port: 3306  MySQL Community Server - GPL.
wait_1          | PORT READY:
basic-wait4ports_wait_1 exited with code 0
```
* in separate console
```sh
docker-compose -f docker-compose.mysql.yml ps
```
```text
------
basic-wait4ports_wait_1   sh -c wait4ports -q -s $TI ...   Up

mysql-server              docker-entrypoint.sh mysqld      Up      3306/tcp, 330
60/tcp

```

and later

```text
basic-wait4ports_wait_1   sh -c wait4ports -q -s $TI ...   Exit 0

mysql-server              docker-entrypoint.sh mysqld      Up       3306/tcp, 33
060/tcp

```

NOTE: during the wait, ond would see misformatted argument output in the ps command:

```sh
docker exec -it  basic-wait4ports_wait_1 sh
```
```sh
ps
```

```text
PID   USER     TIME  COMMAND
    1 root      0:00 wait4ports -q -s 300  tcp //mysql-server 3306
    6 root      0:00 sh
   11 root      0:00 ps
```
and the real command found through `/proc` appears formatted incorrectly:
```sh
cat /proc/1/cmdline
```
```text
wait4ports-q-s300tcp//mysql-server3306/
```
but based on observed behavior everything is working as expected


* alternatively one can suppress `quiet` option in the `Dockerfile` 
```sh
ENTRYPOINT ["sh", "-c", "wait4ports -s $TIMEOUT ${NAME}=tcp://${NODE}:${PORT}"]
```
and see the operational logs of `wait4ports` node:

```text
wait_1          | Trying  (172.19.0.3:3306) ... failed.
wait_1          | Trying  (172.19.0.3:3306) ... failed.
wait_1          | Trying  (172.19.0.3:3306) ... SUCCESS!
basic-wait4ports_wait_1 exited with code 0
```
* Cleanup

```sh
docker-compose -f docker-compose.mysql.yml stop
docker-compose -f docker-compose.mysql.yml rm -f

```

### Note

* cannot use command syntax:
```sh
ENTRYPOINT ["wait4ports", "-s" ,"$TIMEOUT","${NAME}=tcp://${NODE}:${PORT}"]
```
 will lead to the error:
```text
wait_1          | Warning: argument to -s [$TIMEOUT] is not a numeric value, sle
ep value set to 0
wait_1          | ${NODE}:${PORT} getaddrinfo: Invalid argument
wait_1          | No valid peers found in arguments!
wait_1          | Usage: wait4ports [-q] [-s <seconds>] [<name>=]<protocol>://<h
ost>:<port> [...]
```

have to use shell syntax:
```sh
ENTRYPOINT ["sh", "-c", "wait4ports -s $TIMEOUT ${NAME}=tcp://${NODE}:${PORT}"]
```
* Also, one can dynamically set quiet/loud using shell syntax of ENTRYPOINT command  argument:
```sh
ENTRYPOINT ["sh", "-c", "wait4ports ${QUIETFLAG} -s $TIMEOUT ${NAME}=tcp://${NODE}:${PORT}"]
```

### TODO

* consider wait on multiple nodes

### See Also

  * docker-compose [documentation](https://docs.docker.com/compose/startup-order/)  on control startup and shutdown order in Compose that mentions `wait-for-it.sh` script
  * [post](https://medium.com/@krishnaregmi/wait-for-it-docker-compose-f0bac30f3357) on Wait-for-it + Docker Compose
   * [how to use wait-for-it in docker-compose file?](https://stackoverflow.com/questions/63198731/how-to-use-wait-for-it-in-docker-compose-file)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
