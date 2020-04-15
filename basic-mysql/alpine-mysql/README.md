### Info

Build the alpine based  mysql 5.x Docker image:

```sh
cd alpine-mysql
export SERVER_IMAGE=alpine-mysql-server
docker build -f Dockerfile  -t $SERVER_IMAGE .
```
run it with environments matching the `application.properties`:
```sh
export SERVER_NAME=mysql-server
docker run --name $SERVER_NAME -e MYSQL_ROOT_PASSWORD=password -e MYSQL_USER=java -e MYSQL_DATABASE=test -e MYSQL_PASSWORD=password -d $SERVER_IMAGE
```
The enviroment entries `MYSQL_ROOT_PASSWORD`, `MYSQL_USER`,`MYSQL_DATABASE`, `MYSQL_PASSWORD` are required by Mysql docker image.
It will take the Docker instance  quite some time to launch. 
One can safely start building and runing Spring app container while database initializes itself.
Eventually
observe the successful start log message in `mysql-server` container:
```sh
docker logs $SERVER_NAME
```
```sh
[Server] /usr/sbin/mysqld (mysqld 8.0.18) initializing of server in progress as process 45
...
[Server] /usr/sbin/mysqld: ready for connections. Version: '8.0.18'  socket: '/var/run/mysqld/mysqld.sock'  port: 3306  MySQL Community Server - GPL.

[Server] /usr/sbin/mysqld: ready for connections. Version: '8.0.18'  socket: '/var/run/mysqld/mysqld.sock'  port: 3306  MySQL Community Server - GPL.
[Server] X Plugin ready for connections. Socket: '/var/run/mysqld/mysqlx.sock' bind-address: '::' port: 33060
```

verify the console connection:
```sh
docker exec -it $SERVER_NAME mysql -P 3306 -h localhost -u java -ppassword -e "set @var = '1'; select @var ;"
```
