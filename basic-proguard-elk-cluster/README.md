### Info

### Testing
```sh
docker pull kibana:7.17.7
docker pull elasticsearch:7.17.7
docker pull docker.elastic.co/apm/apm-server:7.17.7
docker pull eclipse-temurin:8-jre-alpine
```
optionally
```sh
docker pull alpine:3.14

```
```sh
pushd app1; mvn -Pproguard package;popd; pushd app2; mvn -Pproguard package;popd
```
```sh
find . -iname 'example*jar'
```
```text
./app2/target/example.server_proguard_base.jar
./app2/target/example.server.jar
./app1/target/example.relay.jar
./app1/target/example.relay_proguard_base.jar
```

```sh
df -h /
```
```text
Filesystem      Size  Used Avail Use% Mounted on
/dev/sda1        22G   15G  6.1G  71% /
```

```sh
docker-compose up --build --detach
```

### Update Dockerfiles to Reflect
```text
app1 exited with code 1
app2 exited with code 1
```
```sh
docker-compose ps
```
```text
    Name              Command                 State                 Ports
--------------------------------------------------------------------------------
apm-server      /usr/bin/tini --       Up (healthy)          0.0.0.0:8200-
                /usr/loca ...                                >8200/tcp,:::8200-
                                                             >8200/tcp
app1            java -javaagent:/hom   Up (health:           0.0.0.0:8080-
                e/elas ...             starting)             >8080/tcp,:::8080-
                                                             >8080/tcp, 8888/tcp
app2            java -javaagent:/hom   Restarting
                e/elas ...
elasticsearch   /bin/tini --           Up (healthy)          0.0.0.0:9200-
                /usr/local/bi ...                            >9200/tcp,:::9200-
                                                             >9200/tcp, 9300/tcp
kibana          /bin/tini --           Up (healthy)          0.0.0.0:5601-
                /usr/local/bi ...                            >5601/tcp,:::5601-
                                                             >5601/tcp

```
			
```sh
docker-compose logs app1
```
```text
app1             | 2025-12-26 23:34:00,029 [main] INFO  co.elastic.apm.agent.impl.ElasticApmTracer - Tracer switched to RUNNING state
app1             | Error: Could not find or load main class example.Application
app1             | Caused by: java.lang.ClassNotFoundException: example.Application

```
```sh
java -cp target/example.relay.jar example.Applciation
```
```text
Error: Could not find or load main class example.Applciation
```
```sh
jar tvf target/example.relay.jar  | grep -i META-INF/MANIFEST.MF
```
```text
   403 Fri Dec 26 18:24:04 EST 2025 META-INF/MANIFEST.MF
```
```sh
jar xvf target/example.relay.jar  META-INF/MANIFEST.MF
```
```text
inflated: META-INF/MANIFEST.MF
```
```sh
cat META-INF/MANIFEST.MF
```
```text
Manifest-Version: 1.0
Spring-Boot-Classpath-Index: BOOT-INF/classpath.idx
Implementation-Title: relay
Implementation-Version: 0.6.0-SNAPSHOT
Start-Class: example.Application
Spring-Boot-Classes: BOOT-INF/classes/
Spring-Boot-Lib: BOOT-INF/lib/
Build-Jdk-Spec: 1.8
Spring-Boot-Version: 2.3.4.RELEASE
Created-By: Maven Jar Plugin 3.2.0
Main-Class: org.springframework.boot.loader.JarLauncher
```


```sh
docker-compose rm -f; docker-compose up --build --detach
```
```text
Creating elasticsearch ... done
Creating kibana        ... done
Creating apm-server    ... done
Creating app2          ... done
Creating app1          ... done
```

```sh
docker-compose ps
```
```text
   Name                Command              State                Ports         
--------------------------------------------------------------------------------
apm-server      /usr/bin/tini --          Up (healthy)   0.0.0.0:8200-          
                /usr/loca ...                            >8200/tcp,:::8200-     
                                                         >8200/tcp              
app1            java                      Up (healthy)   0.0.0.0:8080-          
                -javaagent:/home/elas                    >8080/tcp,:::8080-     
                ...                                      >8080/tcp              
app2            /bin/sh -c java           Up (healthy)   8080/tcp               
                -javaagent ...                                                  
elasticsearch   /bin/tini --              Up (healthy)   0.0.0.0:9200-          
                /usr/local/bi ...                        >9200/tcp,:::9200-     
                                                         >9200/tcp, 9300/tcp    
kibana          /bin/tini --              Up (healthy)   0.0.0.0:5601-          
                /usr/local/bi ...                        >5601/tcp,:::5601-     
                                                         >5601/tcp         
```

apparently both exec form of ENTRYPOINT / JSON array argument, work with APM
```sh
ENTRYPOINT [ "java", "-javaagent:/home/elastic-apm-agent.jar", "-Delastic.apm.service_name=relay", "-Delastic.apm.application_packages=example", "-Delastic.apm.server_url=http://apm-server:8200", "-Djava.security.egd=file:/dev/./urandom", "-cp", "/home/relay.jar", "org.springframework.boot.loader.JarLauncher"]
```
and/or
```sh
ENTRYPOINT java -javaagent:/home/elastic-apm-agent.jar -Delastic.apm.service_name=relay -Delastic.apm.application_packages=example -Delastic.apm.server_url=http://apm-server:8200 -Djava.security.egd=file:/dev/./urandom -cp  /home/relay.jar $MAINCLASS
```

```sh
curl -v http://localhost:8080/users/1
```
```text
*   Trying 127.0.0.1:8080...
* Connected to localhost (127.0.0.1) port 8080 (#0)
> GET /users/1 HTTP/1.1
> Host: localhost:8080
> User-Agent: curl/7.81.0
> Accept: */*
> 
* Mark bundle as not supporting multiuse
< HTTP/1.1 502 
< Content-Length: 0
< Date: Sat, 27 Dec 2025 00:03:59 GMT
< 
* Connection #0 to host localhost left intact
```
```sh
curl -X POST -H 'Content-type: application/json' -d '{"id":2, "name":"Bob"}' -v http://localhost:8080/users
```
```text
Note: Unnecessary use of -X or --request, POST is already inferred.
*   Trying 127.0.0.1:8080...
* Connected to localhost (127.0.0.1) port 8080 (#0)
> POST /users HTTP/1.1
> Host: localhost:8080
> User-Agent: curl/7.81.0
> Accept: */*
> Content-type: application/json
> Content-Length: 22
> 
* Mark bundle as not supporting multiuse
< HTTP/1.1 201 
< Content-Type: application/json
< Transfer-Encoding: chunked
< Date: Sat, 27 Dec 2025 00:36:58 GMT
< 
* Connection #0 to host localhost left intact
{"email":null,"id":2,"name":"Bob"}
```

```sh
curl -X PUT -H 'Content-type: application/json' -d '{"id":2, "name":""}' -v http://localhost:8080/users
```
```text
*   Trying 127.0.0.1:8080...
* Connected to localhost (127.0.0.1) port 8080 (#0)
> PUT /users HTTP/1.1
> Host: localhost:8080
> User-Agent: curl/7.81.0
> Accept: */*
> Content-type: application/json
> Content-Length: 19
> 
* Mark bundle as not supporting multiuse
< HTTP/1.1 405 
< Allow: POST
< Content-Type: application/json
< Transfer-Encoding: chunked
< Date: Sat, 27 Dec 2025 00:38:13 GMT
< 
* Connection #0 to host localhost left intact
{"timestamp":"2025-12-27T00:38:13.120+00:00","status":405,"error":"Method Not Allowed","message":"","path":"/users"}
```

```sh
curl -X PUT -H 'Content-type: application/json' -v http://localhost:8080/users/1
```
```text
*   Trying 127.0.0.1:8080...
* Connected to localhost (127.0.0.1) port 8080 (#0)
> PUT /users/1 HTTP/1.1
> Host: localhost:8080
> User-Agent: curl/7.81.0
> Accept: */*
> Content-type: application/json
> 
* Mark bundle as not supporting multiuse
< HTTP/1.1 400 
< Content-Type: application/json
< Transfer-Encoding: chunked
< Date: Sat, 27 Dec 2025 00:41:07 GMT
< Connection: close
< 
* Closing connection 0
{"timestamp":"2025-12-27T00:41:07.059+00:00","status":400,"error":"Bad Request","message":"","path":"/users/1"}
```
```sh
curl -H 'Content-type: application/json' -v http://localhost:8080/users/1
```
```text
*   Trying 127.0.0.1:8080...
* Connected to localhost (127.0.0.1) port 8080 (#0)
> GET /users/1 HTTP/1.1
> Host: localhost:8080
> User-Agent: curl/7.81.0
> Accept: */*
> Content-type: application/json
> 
* Mark bundle as not supporting multiuse
< HTTP/1.1 500 
< Content-Type: application/json
< Transfer-Encoding: chunked
< Date: Sat, 27 Dec 2025 00:42:03 GMT
< Connection: close
< 
* Closing connection 0
{"timestamp":"2025-12-27T00:42:03.212+00:00","status":500,"error":"Internal Server Error","message":"","path":"/users/1"
```

![Transaction](https://github.com/sergueik/springboot_study/blob/master/basic-proguard-elk-cluster/screenshots/transaction.png)


```sh
df  -h /
```
```text
Filesystem      Size  Used Avail Use% Mounted on
/dev/sda1        22G   15G  5.5G  74% /
```

### Cleanup
```
docker-compose stop
```
```text
Stopping app1          ... done
Stopping app2          ... done
Stopping apm-server    ... done
Stopping kibana        ... done
Stopping elasticsearch ... done


```
```sh
docker-compose rm -f
```
```text
Going to remove app1, app2, apm-server, kibana, elasticsearch
Removing app1          ... done
Removing app2          ... done
Removing apm-server    ... done
Removing kibana        ... done
Removing elasticsearch ... done
```
```sh
docker system prune -f
```
```sh
docker image rm basic-proguard-elk-cluster_elasticsearch basic-proguard-elk-cluster_apm-server basic-proguard-elk-cluster_kibana  basic-proguard-elk-cluster_app2 basic-proguard-elk-cluster_app1
```
```sh
docker image ls | grep -E '(temurin|apm|kibana|elastic)'
```
```text
eclipse-temurin                    11-jre-alpine     f135099692b9   6 weeks ago   169MB
eclipse-temurin                    8-jre-alpine      4cfc34e7cef0   6 weeks ago   150MB
elasticsearch                      7.17.7            ec0817395263   3 years ago   619MB
docker.elastic.co/apm/apm-server   7.17.7            79ccb403f58f   3 years ago   258MB
kibana                             7.17.7            47c5b6ca1535   3 years ago   799MB
```
### See Also

  * https://www.elastic.co/docs/reference/apm/agents/java
