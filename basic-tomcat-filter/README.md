### Info

This directory contins a replica of the project  [](https://github.com/ggrandes/headers-servlet-filter)

It is used with Docker basic  tomcat image

### Testing
```sh
docker pull davidcaste/alpine-tomcat
```
```sh
docker run -p 8080:8080 -it --rm davidcaste/alpine-tomcat /opt/tomcat/bin/catalina.sh run
```
```
netstat -antp | grep 8080
tcp6       0      0 :::8080                 :::*                    LISTEN      4159/docker-proxy

```
```sh
 curl -I -k http://localhost:8080/
HTTP/1.1 404
Transfer-Encoding: chunked
Date: Wed, 25 Mar 2020 17:51:42 GMT
```
```sh
docker exec -it $(docker container ls | grep 'davidcaste/alpine-tomcat'| awk '{print $1}') sh

```
```sh
export CONTAINER=$(docker container ls | grep 'davidcaste/alpine-tomcat'| awk '{print $1}')
docker cp $CONTAINER:/opt/tomcat/conf/web.xml .
docker cp ./sample.war $CONTAINER:/opt/tomcat/webapps/
```

```sh
docker build -t example-tomcat-filter -f Dockerfile  .
```
### See  Also
https://www.journaldev.com/1933/java-servlet-filter-example-tutorial
http://www.avajava.com/tutorials/lessons/what-is-a-filter-and-how-do-i-use-it.html%3Fpage%3D2?page=1
https://hub.docker.com/r/davidcaste/alpine-tomcat
https://github.com/davidcaste/docker-alpine-tomcat/blob/master/tomcat8/Dockerfile.jre8
https://www.moreofless.co.uk/static-content-web-pages-images-tomcat-outside-war/
