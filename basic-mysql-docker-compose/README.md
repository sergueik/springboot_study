### Info

Basic docker-compose tomcat8 with pre-built pure jsp tomcat application interacting with mysql data source project cloned from
[springboot mySQL Docker container](https://github.com/dmulligan/docker-example-tomcat-mysql) converted to run on alpine openjdk jre base image.

### Usage

```sh
 docker-compose up
```
followed by
```sh
lynx http://localhost/example-webapp
```

currentlly shows `500 – Internal Server Error` on JSP Datasource and JNI  pages because not properly configured

To troubleshoot poor thing

```sh
NAME='_web_'
ID=$(docker ps  | grep $NAME |awk '{print $1}')
```
it appears that
```sh
docker logs $ID
```
is not helping much

```sh
docker exec -it $ID sh
```

then
```sh
ps ax |  grep catalin[a]
```
figure the cataliba base from the command
```sh
```
then
```sh
tail /usr/local/tomcat/logs/localhost.$(date +'%Y-%m-%d').log
```

Download mysql jdbc driver directly
```sh
wget https://dev.mysql.com/get/Downloads/Connector-J/mysql-connector-java-8.0.20.tar.gz
tar xzvf mysql-connector-java-8.0.20.tar.gz
docker cp $(find mysql-connector-java-8.0.20/ -iname 'mysql-co*jar') $ID:/usr/local/tomcat/lib
mkdir tomcat/webapps/example-webapp/WEB-INF/lib

wget https://dev.mysql.com/get/Downloads/Connector-J/mysql-connector-java-5.1.49.tar.gz
tar xzvf mysql-connector-java-5.1.49.tar.gz
docker cp $(find mysql-connector-java-5.1.49/ -iname 'mysql-co*bin.jar') $ID:/usr/local/tomcat/lib

cp $(find mysql-connector-java-8.0.20/ -iname 'mysql-co*jar') tomcat/webapps/example-webapp/WEB-INF/lib/
```
# Unsupported major.minor version 52.0 
# is java 7
# https://dev.mysql.com/doc/connector-j/8.0/en/connector-j-versions.html
to enable additional logging
```sh
docker cp $ID:/usr/local/tomcat/conf/logging.properties .
sed -i 's|NICE|DEBUG|g' logging.properties
docker cp logging.properties $ID:/usr/local/tomcat/conf/
```

```sh
nc -z -p 3606 172.17.0.3
```
NAME=_db_
ID=$(docker ps  | grep $NAME |awk '{print $1}')
docker exec -it $ID sh
```

in terminal docker-compose is run one sees
```sh
delayed_start    | Waiting on the mysql_db 3306
delayed_start    | Waiting 60 sec
mysql_db         | 2020-06-11 13:00:53+00:00 [Note] [Entrypoint]: Database files initialized
delayed_start    | Waiting on the mysql_db 3306
delayed_start    | Waiting 60 sec
mysql_db         | 2020-06-11 13:00:53+00:00 [Note] [Entrypoint]: Starting temporary server
```
and so on until finally
```sh
mysql_db         | 2020-06-11T13:03:01.452895Z 0 [System] [MY-010931] [Server] /usr/sbin/mysqld: ready for connections. Version: '8.0.18'  socket: '/var/run/mysqld/mysqld.sock'  port: 3306  MySQL Community Server - GPL.
mysql_db         | 2020-06-11T13:03:01.752552Z 0 [System] [MY-011323] [Server] X Plugin ready for connections. Socket: '/var/run/mysqld/mysqlx.sock' bind-address: '::' port: 33060
delayed_start    | Waiting on the mysql_db 3306
delayed_start    | Got Response
delayed_start exited with code 0
```

### Cleanup
```sh
for N in web db ; do docker stop $(docker ps  | grep _${N}_ |awk '{print $1}'); done
docker-compose stop
docker container prune -f
```


### See Also

 * advanced `docker-compose.yaml` syntax example [clustering nginx/tomct](https://github.com/jistol/docker-compose-nginx-tomcat-clustering-sample/blob/master/docker-compose.yml)
https://github.com/jistol/docker-compose-nginx-tomcat-clustering-sample/blob/master/docker-compose.yml
