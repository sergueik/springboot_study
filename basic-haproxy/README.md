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
for NUM in $(seq 1 1 3) ; do APP_SERVER="application-server${NUM}"; docker container stop $APP_SERVER;  docker container rm -f $APP_SERVER; docker run -p 808${NUM}:8080 -d --env "APP_SERVER=${APP_SERVER}" --name $APP_SERVER davidcaste/alpine-tomcat /opt/tomcat/bin/catalina.sh run ; done
```
pull and build the proxy
```sh
IMAGE=proxy
docker build -t $IMAGE -f Dockerfile.$IMAGE .
```
```sh
CONTAINER='proxy-example'
docker rm -f $CONTAINER
docker run --name $CONTAINER -p 8086:8080 --link application-server1 --link application-server2 --link application-server3 -v $(pwd)/haproxy.cfg:/usr/local/etc/haproxy/haproxy.cfg:ro -d $IMAGE
docker logs $CONTAINER
```			
* build Application war(s) and install on application server(s)
```sh
pushd application 
mvn clean package
popd
```
```sh
for NUM in $(seq 1 1 3) ; do APPDIR="app${NUM}";APP_SERVER="application-server${NUM}"; docker cp application/target/dummy.war $APP_SERVER:/opt/tomcat/webapps/${APPDIR}.war ; done
```

```sh
for NUM in $(seq 1 1 3) ; do APP_SERVER="application-server${NUM}"; docker stop $APP_SERVER; docker start $APP_SERVER; done
```

verify connecting directly

```sh
curl -k http://localhost:8082/app2/index.jsp
```
or
```sh
curl -k http://localhost:8083/app3/
```

* connect to proxy which will redirect to the application server
```sh
curl -k  http://localhost:8086/app1/index.jsp
```

this will reponse with:




```sh
<html><body><pre>Server:82b062bc16e4
Request URL: http://localhost:8086/app1/
APP_SERVER = application-server1
</pre></body></html>
```
and log the haproxy operation like
```sh
 docker logs $CONTAINER
```
```sh
 Proxy http started.
172.17.0.1:42696 [12/Aug/2020:03:32:13.199] http appservers/appserver1 0/0/0/5/5 200 350 - - ---- 1/1/0/0/0 0/0 "GET /app1/index.jsp HTTP/1.1"
```
and
```sh
curl -k  http://localhost:8086/app2/
```


```sh

<html><body><pre>Server:ec50d1fce7ca
Request URL: http://localhost:8086/app2/
APP_SERVER = application-server2
</pre></body></html>
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
 * [HAProxy Crash Course](https://www.youtube.com/watch?v=qYnA2DFEELw)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
