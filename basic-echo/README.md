### Info
__Beam Echo Server__ Docker example (java)
To support the synchronization between slowly booting Docker nodes (ones that perform a time-consuming initialization in run phase).
The basic exercise echo server class can be placed into the JRE-based Docker image and executed from `ENTRYPOINT` or `RUN` (WIP, need to add forking to make non blocking) after the lengthy initialization is finished.
The dependent node then have its `ENTRYPOINT` modified to become a shell script checking the port availability across the network. Optionally sending a `QUIT` message shuts down the beam server. For dependency roles that host  slow initializing apps (MySQL, Mongo etc) that appear visible in the cluster and listening to public port by default, the app itself is the beam server.

### Test

* run locally
```sh
javac EchoServer.java
```
* test locally
```sh
export SERVICE_PORT=8889
java EchoServer
```
(from the separate console)
```sh
nc -z localhost 8889; echo $?
```
```sh
0
```
```sh
echo QUIT| telnet 127.0.0.1 8889
```

```sh
Trying 127.0.0.1...
Connected to 127.0.0.1.
Connection closed by foreign host.
```
in the first console:
```sh
Started server on port 8889
Accepted connection from 127.0.0.1
Received QUIT
Stopping accepting the connections
Closing server
```
* run in container
```sh

docker build -f Dockerfile -t basic-echo .
docker run -e SERVICE_PORT=8085 -p 8085:8085 basic-echo
```
test dockerized
```sh
nc -z localhost 8085
echo HELLO| telnet 127.0.0.1 8085
echo QUIT| telnet 127.0.0.1 8085
```
```sh
Started server on port 8085
Accepted connection from 172.17.0.1
Accepted connection from 172.17.0.1
Received QUIT
Stopping accepting the connections
Closing serve
```
destroy all started containers and image afterwards
```sh
docker container prune -f
docker image prune -f
```

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
