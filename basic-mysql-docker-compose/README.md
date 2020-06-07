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
cp $(find mysql-connector-java-8.0.20/ -iname 'mysql-co*jar') tomcat/webapps/example-webapp/WEB-INF/lib/

```

```sh
nc -z -p 3606 172.17.0.3
```
NAME=_db_
ID=$(docker ps  | grep $NAME |awk '{print $1}')
docker exec -it $ID sh
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
