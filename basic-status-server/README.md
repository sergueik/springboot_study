### Info



minimal http server for Minikube/Argo CD learning exercise controllable HTTP application whose liveness and readiness responses can be changed through container environment variables, rather than introducing Spring Boot, a database, or an ingress controller just to demonstrate Kubernetes behavior 


### Usage
```sh
docker pull alpine/java:11-jre
```

```sh
docker build -f Dockerfile -t status-server .
```

```text
Sending build context to Docker daemon  46.08kB
Step 1/5 : FROM alpine/java:11-jre
 ---> 468a214d75b6
Step 2/5 : ARG app_jar="example.status-server.jar"
 ---> Using cache
 ---> 43b242648e06
Step 3/5 : ADD "target/${app_jar}" app.jar
 ---> Using cache
 ---> ca815850e151
Step 4/5 : EXPOSE 8080
 ---> Using cache
 ---> 3583cff9b5e2
Step 5/5 : ENTRYPOINT java -cp app.jar example.App -ready ${READY:up}  -live ${LIVE:up}
 ---> Running in 58eeab62e6a6
Removing intermediate container 58eeab62e6a6
 ---> c2fa2f5dd60d
Successfully built c2fa2f5dd60d
Successfully tagged status-server:latest
```
```sh
docker run --name status-server -d -e LIVE=up -e READY=down -p 8081:8080 status-server
```

```sh
docker logs status-server
```
```text
Server started on port 8080
```
```sh
curl -I http://$(docker-machine ip):8081/api/ready
```
```
HTTP/1.1 503 Service Unavailable
Date: Sat, 24 Oct 2026 13:38:54 GMT
```
```
curl -I http://$(docker-machine ip):8081/api/live
```
```text
HTTP/1.1 200 OK
Date: Sat, 24 Oct 2026 13:42:48 GMT
```
```sh
docker stop status-server
docker container prune -f
```

### Note
Replace base image with bellsoft's. This yields a small nice present

```sh
docker pull bellsoft/liberica-openjre-alpine:8
```
```text
8: Pulling from bellsoft/liberica-openjre-alpine
55afa1ecc21d: Pull complete
44af7b7a294c: Pull complete
29a9756cd656: Pull complete
Digest: sha256:a537e3794dde32a8899f0db1a0e045db4e1634a351bf35d3a0c9d8fed7faa4f2
Status: Downloaded newer image for bellsoft/liberica-openjre-alpine:8
docker.io/bellsoft/liberica-openjre-alpine:8
```
```sh
docker build -f Dockerfile -t status-server .
```

```sh
docker run --name status-server -d -e LIVE=up -e READY=down -p 8081:8080 status-server
```
```text
34e4ae4f6d17b0856219b3c8e050aaa7dc93ef02d06b0b6772524e1bf2400523
```
```sh
curl -I http://$(docker-machine ip):8081/api/ready
```
```text
curl: (7) Failed to connect to 192.168.99.106 port 8081 after 2004 ms: Could not connect to server
```
kouzm@sergueik23 MINGW64 /c/developer/sergueik/springboot_study/basic-status-server (master)
```sh
docker container ls
```
```text
CONTAINER ID        IMAGE               COMMAND             CREATED             STATUS              PORTS               NAMES
```
docker logs 34e4
```
Error occurred during initialization of VM
Cannot create VM thread. Out of system resources.
```
this is not caused by the code:
```sh
docker run --rm bellsoft/liberica-openjre-alpine:8 java -version
```
```text
Error occurred during initialization of VM
Cannot create VM thread. Out of system resources.

```
### TODO

Suppress
```text
WARNING: sendResponseHeaders: being invoked with a content length for a HEAD request
```



