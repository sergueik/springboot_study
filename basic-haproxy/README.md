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
```sh
IMAGE=proxy
docker build -t $IMAGE -f Dockerfile.$IMAGE .
```
```sh
CONTAINER='proxy-example'
docker rm -f $CONTAINER
docker run --name $CONTAINER  -p 8080:8080 -v $(pwd)/haproxy.cfg:/usr/local/etc/haproxy/haproxy.cfg:ro -d $IMAGE
docker logs $CONTAINER
```			

### See Also
 * [haproxy load balanced web application server cluster](https://github.com/ianblenke/tutum-docker-clusterproxy) with discovery implemented in custom Python script
 * consul-template haproxy round-robin scalable [setup](https://github.com/camptocamp/docker-consul-demo)
 

