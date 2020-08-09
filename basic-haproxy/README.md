### Info

### Usage
* tesh locally
```sh
mvn clean tomcat:run-war
curl -I http://localhost:8080/redirector/index.html
```
this will redirect to application:
```sh
HTTP/1.1 302 Moved Temporarily
Server: Apache-Coyote/1.1
Location: http://localhost:8080/About
```
build UI frontend into Docker container based on tomcat image:

```sh
IMAGE='frontend'
docker build -t $IMAGE -f Dockerfile.$IMAGE .
```
* and run
```sh
CONTAINER='frontend-example'
docker rm -f $CONTAINER
docker run --name $CONTAINER  -p 8080:8080 -e REDIRECT_HOST=other_host -d $IMAGE
```
* exercise  the frontend application:
```sh
curl -I http://localhost:8080/redirector/index.html
```
this will redirect to randomly chosen app path on `REDIRECT_HOST`:
```sh
HTTP/1.1 302 Found
Server: Apache-Coyote/1.1
Location: http://other_host:8080/About
Content-Type: text/html
Content-Length: 0
```
* build the proxy node container
```sh
docker pull haproxy:1.8-alpine
```
create blank application server(s)
```sh
docker run -p 8085:8080 -d --name application-server davidcaste/alpine-tomcat /opt/tomcat/bin/catalina.sh run
```
pull and build the proxy
```sh
IMAGE=proxy
docker build -t $IMAGE -f Dockerfile.$IMAGE .
```
```sh
CONTAINER='proxy-example'
docker rm -f $CONTAINER
docker run --name $CONTAINER -p 8080:8080 --link application-server -v $(pwd)/haproxy.cfg:/usr/local/etc/haproxy/haproxy.cfg:ro -d $IMAGE
docker logs $CONTAINER
```			
* build Application war(s) and install on application server(s)
```sh
pushd webapp 
mvn clean package
popd
```
```sh
docker cp webapp/target/dummy.war application-server:/opt/tomcat/webapps/ROOT.war
```
```sh
doker stop application-server
doker start application-server
```
* connect to proxy which will redirect to the application server

### See Also
 * [haproxy load balanced web application server cluster](https://github.com/ianblenke/tutum-docker-clusterproxy) with discovery implemented in custom Python script
 * consul-template haproxy round-robin scalable [setup](https://github.com/camptocamp/docker-consul-demo)
 * haproxy with rsyslog [project](https://github.com/mminks/haproxy-docker-logging) 

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
