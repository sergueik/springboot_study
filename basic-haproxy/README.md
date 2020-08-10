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
Location: http://other_host:8080/About
```
```sh
curl -I http://localhost:8080/redirector/index.html
```
```sh
HTTP/1.1 302 Found
Location: http://other_host:8080/Contact
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
docker run --name $CONTAINER -p 8086:8080 --link application-server -v $(pwd)/haproxy.cfg:/usr/local/etc/haproxy/haproxy.cfg:ro -d $IMAGE
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
docker cp webapp/target/dummy.war application-server:/opt/tomcat/webapps/products.war
```
```sh
docker stop application-server
docker start application-server
```
* connect to proxy which will redirect to the application server
```sh
curl http://localhost:8086/products/index.jsp
```

```sh
curl http://localhost:8086/
```
The page will display a lot of information:

```sh
Server:939cffb58551
Request URL: http://localhost:8080/products/index.jsp
PATH = /usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/opt/jdk/bin
JAVA_HOME = /opt/jdk
CATALINA_OUT = /dev/null
CATALINA_HOME = /opt/tomcat
XFILESEARCHPATH = /usr/dt/app-defaults/%L/Dt
GLIBC_VERSION = 2.23-r3
LANG = C.UTF-8
JAVA_VERSION_MAJOR = 8
TOMCAT_MAJOR = 8
TOMCAT_VERSION = 8.5.3
HOSTNAME = 939cffb58551
NLSPATH = /usr/dt/lib/nls/msg/%L/%N.cat
JAVA_VERSION_MINOR = 92
TOMCAT_HOME = /opt/tomcat
PWD = /
JAVA_PACKAGE = server-jre
JAVA_VERSION_BUILD = 14
HOME = /root
SHLVL = 1
Request Parameters
******************

Request Headers
***************
Name  : 'host'
Class : java.lang.String
String: 'localhost:8080'

Name  : 'user-agent'
Class : java.lang.String
String: 'curl/7.58.0'

Name  : 'accept'
Class : java.lang.String
String: '*/*'
```

in particular the hostname of the application-server (evaluated through `System.getenv().get("HOSTNAME")` and `java.net.InetAddress.getLocalHost().getHostName()`. This helps tracking the load balancing (booting additional application servers and integrating with frontend node is work in progress).


* rebuild the `frontend-example` container linking it to `proxy-example`:
```sh
IMAGE='frontend'
docker build -t $IMAGE -f Dockerfile.$IMAGE .
CONTAINER='frontend-example'
docker rm -f $CONTAINER
docker run --name $CONTAINER --link proxy-example --link application-server -p 8080:8080 -e REDIRECT_HOST=proxy-example -d $IMAGE
```
then execute curl request to 
```sh
curl  -k  http://localhost:8080/redirector/index.html
```
these will get redirected to `proxy_example` port `8080` and routed to whatever is configured in `haproxy.conf` there (this is work in progress:  a lot of empty response observed)
### See Also
 * [haproxy load balanced web application server cluster](https://github.com/ianblenke/tutum-docker-clusterproxy) with discovery implemented in custom Python script
 * consul-template haproxy round-robin scalable [setup](https://github.com/camptocamp/docker-consul-demo)
 * haproxy with rsyslog [project](https://github.com/mminks/haproxy-docker-logging) 
 * introduction to [HAProxy Logging](https://www.haproxy.com/blog/introduction-to-haproxy-logging/)	

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
