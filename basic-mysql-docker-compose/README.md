### Info

Basic docker-compose tomcat8 with pre-built pure jsp tomcat application interacting with mysql data source project cloned from
[springboot mySQL Docker container](https://github.com/dmulligan/docker-example-tomcat-mysql) converted to run on alpine openjdk jre base image.

### Usage

```sh
export COMPOSE_HTTP_TIMEOUT=600
docker-compose up
docker container stop $(docker container ls | grep '_web_' | awk '{print $1}') 
docker-compose up
```
followed by
```sh
lynx http://localhost/example-webapp
```

The __Datasource__ and __JNI__ pages work on the second launch but __JDBC__ shows `500 – Internal Server Error`
```sh
java.sql.SQLException: The url cannot be null
```
presumably because the environment settings  from `docker-compose.yaml` not properly proparated into tomcat application.

On a first launch all 3 pages: __JDBC__, __JNI__ and __DataSource__  show 500 with similar error.


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
### To Run Without docker-compose

* pull base images
```sh
docker pull mysql:5.7
```
```sh
docker pull tomcat:8.0-alpine 
```

modify the 
`tomcat/webapps/example-webapp/META-INF/context.xml`
with the chosen dns name of the database server.

* notice: this project is mapped into the containers:

```sh
docker run --name mysql-server -e MYSQL_ROOT_PASSWORD=nimda -e MYSQL_USER=example_db_user -e MYSQL_DATABASE=example_db -e MYSQL_PASSWORD=example_db_pass -v db:/docker-entrypoint-initdb.d  -d mysql:5.7
```
and
```sh
docker run --link mysql-server --name web-server -e JDBC_URL='jdbc:mysql://mysql-server:3306/example_db?connectTimeout=0&amp;socketTimeout=0&amp;autoReconnect=true' -e JDBC_USER=example_db_user -e JDBC_PASS=example_db_pass -p 80:8080  -v $(pwd)/tomcat/webapps:/usr/local/tomcat/webapps -d tomcat:8.0-alpine
```
* verify
```sh
 curl http://localhost:80/example-webapp/test-jndi.jsp
```
```html
<html>
<body>
<pre>
DataSource: org.apache.tomcat.dbcp.dbcp2.BasicDataSource@5a04602f
Connection: 1615708347, URL=jdbc:mysql://mysql-server:3306/example_db?connectTimeout=0&socketTimeout=0&autoReconnect=true, UserName=example_db_user@172.17.0.3, MySQL Connector Java
</pre>
</body>
</html>
```
```sh
java.sql.SQLException: null,  message from server: "Host '172.18.0.3' is not allowed to connect to this MySQL server
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
