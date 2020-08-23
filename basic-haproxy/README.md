### Info
This directory contains toy cluster to prectice the haproxy
[ACLs](https://www.haproxy.com/blog/introduction-to-haproxy-acls/
### Usage
#### Front End (optional)
* tesh locally
```sh
pushd frontend
mvn clean tomcat:run-war
popd
curl -I http://localhost:8080/redirector/index.html
```
this will redirect to application:
```sh
HTTP/1.1 302 Moved Temporarily
Server: Apache-Coyote/1.1
Location: http://localhost:8080/app1/index.jsp
```
* build UI frontend into Docker container based on tomcat image:

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
Location: http://other_host:8080/app3/index.jsp
```
#### Application Container Server Farm
* create blank application container server(s) with a vanilla tomcat intending to deploy war(s) inside later
```sh
for NUM in $(seq 1 1 3) ; do APP_SERVER="application-server${NUM}"; docker container stop $APP_SERVER;  docker container rm -f $APP_SERVER; docker run -p 808${NUM}:8080 -d --env "APP_SERVER=${APP_SERVER}" --name $APP_SERVER davidcaste/alpine-tomcat /opt/tomcat/bin/catalina.sh run ; done
```
- the application servers must be resolvable for proxy container to launch.
* alternatively may run basic Springboot Spring MVC with a basic Thymeleaf layout (see `../basic-static` proejct directory).


#### Proxy
* build the proxy container
```sh
docker pull haproxy:2.1.7-alpine
```
pull and build the proxy
```sh
IMAGE=proxy
docker build -t $IMAGE -f Dockerfile.$IMAGE .
```
```sh
CONTAINER='proxy-example'
docker rm -f $CONTAINER
docker run --name $CONTAINER -p 8086:8080 -p 1936:1936 --link application-server1 --link application-server2 --link application-server3 -v $(pwd)/haproxy.cfg:/usr/local/etc/haproxy/haproxy.cfg:ro -d $IMAGE
docker logs $CONTAINER
```
will say
```sh
Proxy http started.
```	
#### Deploy Applications
* build Application war(s) and install on application server(s)
```sh
pushd application
mvn clean package
popd
```
```sh
for NUM in $(seq 1 1 3) ; do APPDIR="app${NUM}";APP_SERVER="application-server${NUM}"; docker cp application/target/dummy.war $APP_SERVER:/opt/tomcat/webapps/${APPDIR}.war ; done
```
* restart aplication containers
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
CONTAINER=haproxy-example
docker logs $CONTAINER
```
```sh
Proxy http started.
172.17.0.1:42696 [12/Aug/2020:03:32:13.199] http appservers/appserver1 0/0/0/5/5 200 350 - - ---- 1/1/0/0/0 0/0 "GET /app1/index.jsp HTTP/1.1"
```
and
```sh
curl -k  http://localhost:8086/app2/ 2>/dev/null| lynx -stdin -dump | head -10
```

```sh
Server:ec50d1fce7ca
Request URL: http://localhost:8086/app2/
APP_SERVER = application-server2
```
in particular the hostname of the application server servicing the request,
evaluated through `System.getenv().get("HOSTNAME")` or
`java.net.InetAddress.getLocalHost().getHostName()`. This helps tracking the load balancing (booting additional application servers and integrating with frontend node is work in progress).


* rebuild the `frontend-example` container linking it to `proxy-example`:
```sh
IMAGE='frontend'
docker build -t $IMAGE -f Dockerfile.$IMAGE .
CONTAINER='frontend-example'
docker rm -f $CONTAINER
docker run --name $CONTAINER --link proxy-example --link application-server1 --link application-server2 --link application-server3 -p 8080:8080 -e REDIRECT_HOST=localhost -e REDIRECT_PORT=8086 -d $IMAGE
```
then execute curl request to
```sh
curl -L -k  http://localhost:8080/redirector/index.html
```
this will redirect to a random `app1`,`app2`,`app3` on proxy server which will load balance e.g.
```sh


<html><body><pre>Server:90ee030d8bb5
Request URL: http://localhost:8086/app1/index.jsp
APP_SERVER = application-server1
</pre></body></html>

```
these will get redirected to `proxy_example` port `8080` and routed to whatever is configured in `haproxy.conf` there
(this is work in progress:  a lot of empty response observed)

#### Headers
Pass request headers:
```sh
curl -k -I  -H "x-application: something" -k  http://localhost:8086/app2/index.jsp
```
this will produce
```sh
HTTP/1.1 404
transfer-encoding: chunked
date: Mon, 17 Aug 2020 23:48:52 GMT
```
because on the `application-server3` there is no `/app2` application, and the acl
```sh
  acl acl4 req.hdr(x-application) -m found
  use_backend app3 if acl4
```
seems to intruct haproxy to direct to backend `app3`.
* build and deploy `app2` to `application-server3`:
```sh
docker cp application/target/dummy.war  application-server3:/opt/tomcat/webapps/app2.war
docker stop application-server3; docker start application-server3
```
* verify the application response by directly accessing it:
```sh
curl -k -I  -H "x-application: something" -k  http://localhost:8083/app2/index.jsp
```
this will respond now with
```sh
HTTP/1.1 200
Set-Cookie: JSESSIONID=772EF4D6C2F8A284DFCC5A8ED70EF92B;path=/app2/;HttpOnly
Content-Type: text/html;charset=ISO-8859-1
Transfer-Encoding: chunked
Date: Tue, 18 Aug 2020 00:03:26 GMT
```
* repeat through haproxy:
```sh
curl -k -I  -H "x-application: something" -k  http://localhost:8086/app2/index.jsp
```
result will be the same
```sh
HTTP/1.1 200
set-cookie: JSESSIONID=80651A23B54355CC504ADEE6B024E6C2;path=/app2/;HttpOnly
content-type: text/html;charset=ISO-8859-1
transfer-encoding: chunked
date: Tue, 18 Aug 2020 00:03:34 GMT
```
### See Also
 * consul-template haproxy round-robin scalable [setup](https://github.com/camptocamp/docker-consul-demo)
 * haproxy with rsyslog [project](https://github.com/mminks/haproxy-docker-logging)
 * introduction to haproxy [logging](https://www.haproxy.com/blog/introduction-to-haproxy-logging/)	
 * haproxy [crash course](https://www.youtube.com/watch?v=qYnA2DFEELw)
 * container node [](https://github.com/andrewmunsell/haproxy-ui)
 * haproxy [stats](https://www.haproxy.com/blog/exploring-the-haproxy-stats-page/)
 * [haproxy load balanced web application server cluster](https://github.com/ianblenke/tutum-docker-clusterproxy) with discovery implemented in custom Python script
 * configuring [logging](https://stackoverflow.com/questions/44137797/how-to-get-logs-in-a-haproxy1-5-alpine-docker-container) in older HAProxy builds
 * haproxy documentation [memo](https://habr.com/ru/company/ostrovok/blog/438966/) (in Russian)
 * `haproxy.conf` [documentation](https://habr.com/ru/sandbox/34354/)(in Russian)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

