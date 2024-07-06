### Info

This directory contains a replica of [consul springboot microservices project](https://github.com/guedim/spring-projects/tree/master/consul-microservice-discovery-sample) used to dissect how exactly `org.springframework.cloud.cient.discovery.DiscoveryClient` does consul service definition and booostrap.

### Testing
 
The cluster is bootstrapped successfully via
step-by-step packaging of both api service applications
```sh
cd portfolio
mvn clean package
docker build -f Dockerfile -t 'test/portafolio' .
```
ignore errors
```text
Jul 06, 2024 2:51:55 PM org.glassfish.jersey.internal.Errors logErrors
WARNING: The following warnings have been detected: WARNING: HK2 service reification failed for [org.glassfish.jersey.message.internal.DataSourceProvider] with an exception:
MultiException stack 1 of 2
...
```
NOTE: on Linux host the `docker build` step is unnecessary sinc `docker-maven-plugin` does it on its own:
```sh
docker images test/portfolio:0.3.0-SNAPSHOT
```
```text
REPOSITORY       TAG              IMAGE ID       CREATED         SIZE
test/portfolio   0.3.0-SNAPSHOT   786ec6e5a07f   3 minutes ago   136MB
```
```sh
cd ..
cd pricing
mvn clean package
docker build -f Dockerfile -t 'test/pricing' .
```
After the `docker` command is run, the images with `latest` tag will be created:
```sh
docker image ls | grep test/pricing
```
```text
test/pricing        latest            07d0a627068e   53 seconds ago   136MB
test/pricing        0.3.0-SNAPSHOT    63c361da2f78   2 minutes ago    136MB
``` 
followed by the
```sh
cd ..
docker pull progrium/consul
docker-compose -f docker-compose.yaml up
```
which logs to console messages from consul node first

```sh
Creating network "consul_default" with the default driver
Creating consul_discovery_1 ... done
Creating consul_portafolio_api_1 ... done
Creating consul_pricing_api_1    ... done
Attaching to consul_discovery_1, consul_portafolio_api_1, consul_pricing_api_1
discovery_1           | ==> WARNING: Bootstrap mode enabled! Do not enable unless necessary
discovery_1           | ==> WARNING: It is highly recommended to set GOMAXPROCS higher than 1
discovery_1           | ==> Starting raft data migration...
discovery_1           | ==> Starting Consul agent...
discovery_1           | ==> Starting Consul agent RPC...
discovery_1           | ==> Consul agent running!

```
followed with the massive burst of usual Spring messages
which also will log

```text
portafolio_api_1  | 2024-07-06 19:01:58.712  INFO 1 --- [           main] o.s.c.c.s.ConsulServiceRegistry          : Registering service with consul: NewService{id='portfolio-service-57116', name='portfolio-service', tags=[secure=false], address='27f1edd8aac1', meta={}, port=57116, enableTagOverride=null, check=Check{script='null', dockerContainerID='null', shell='null', interval='10s', ttl='null', http='http://27f1edd8aac1:57116/actuator/health', method='null', header={}, tcp='null', timeout='null', deregisterCriticalServiceAfter='null', tlsSkipVerify=null, status='null', grpc='null', grpcUseTLS=null}, checks=null}
```
```text
pricing_api_1     | 2024-07-06 19:01:58.151  INFO 1 --- [           main] o.s.c.c.s.ConsulServiceRegistry          : Registering service with consul: NewService{id='pricing-service-57216', name='pricing-service', tags=[secure=false], address='e936ae9b29b1', meta={}, port=57216, enableTagOverride=null, check=Check{script='null', dockerContainerID='null', shell='null', interval='10s', ttl='null', http='http://e936ae9b29b1:57216/actuator/health', method='null', header={}, tcp='null', timeout='null', deregisterCriticalServiceAfter='null', tlsSkipVerify=null, status='null', grpc='null', grpcUseTLS=null}, checks=null}
```

One can see the services in Consul UI

![Docker Cluster](https://github.com/sergueik/springboot_study/blob/master/consul/screenshots/capture-consil.png)

### Validation
Observed the cluster to be up 
```sh
docker container ls
```
to show
```
CONTAINER ID   IMAGE                    COMMAND                  CREATED          STATUS          PORTS                                                                                                                                                                                                          NAMES
e936ae9b29b1   test/pricing             "sh -c 'java $JAVA_O…"   10 minutes ago   Up 10 minutes   57116/tcp, 0.0.0.0:57216->57216/tcp, :::57216->57216/tcp                                                                                                                                                       consul_pricing_api_1
27f1edd8aac1   test/portafolio          "sh -c 'java $JAVA_O…"   10 minutes ago   Up 10 minutes   0.0.0.0:57116->57116/tcp, :::57116->57116/tcp                                                                                                                                                                  consul_portafolio_api_1
123f10543390   progrium/consul:latest   "/bin/start -server …"   10 minutes ago   Up 10 minutes   53/tcp, 0.0.0.0:8300->8300/tcp, :::8300->8300/tcp, 0.0.0.0:8400->8400/tcp, :::8400->8400/tcp, 8301-8302/tcp, 0.0.0.0:8500->8500/tcp, :::8500->8500/tcp, 8301-8302/udp, 0.0.0.0:8600->53/udp, :::8600->53/udp   consul_discovery_1
```
connecting to the `consul` container
```sh
NAME='consul_discovery'
IMAGE=$(docker container ls | grep  $NAME |awk '{print $1}')
docker exec -it $IMAGE '/bin/bash'
```
```sh
apk update
apk add jq
```
one can find the consul configuration
```sh
jq '.' /config/consul.json
``` 
```JSON
{
  "data_dir": "/data",
  "ui_dir": "/ui",
  "client_addr": "0.0.0.0",
  "ports": {
    "dns": 53
  },
  "recursor": "8.8.8.8",
  "disable_update_check": true
}
```
and just-generated service definitions in `/data/services/`:

```sh
find  /data/services -type f -exec jq '.' {} \;
```
```json

{
  "Token": "",
  "Service": {
    "ID": "pricing-service-57216",
    "Service": "pricing-service",
    "Tags": [
      "secure=false"
    ],
    "Address": "e936ae9b29b1",
    "Port": 57216
  }
}
```
```json
{
  "Token": "",
  "Service": {
    "ID": "portfolio-service-57116",
    "Service": "portfolio-service",
    "Tags": [
      "secure=false"
    ],
    "Address": "27f1edd8aac1",
    "Port": 57116
  }
}
```

### Additional Testing

followed by consul check
```sh
2>/dev/null curl http://127.0.0.1:8500/v1/health/checks/portfolio-service | jq '.'
```	
the detail of the output will be Spring Cloud version specific

```JSON
[
  {
    "Node": "127.0.0.1.xip.io",
    "CheckID": "service:portfolio-service-57116",
    "Name": "Service 'portfolio-service' check",
    "Status": "passing",
    "Notes": "",
    "Output": "HTTP GET http://27f1edd8aac1:57116/actuator/health: 200  Output: {\"status\":\"UP\"}",
    "ServiceID": "portfolio-service-57116",
    "ServiceName": "portfolio-service"
  }
]

```
```JSON
[
  {
    "CONTAINER": "127.0.0.1.xip.io",
    "CheckID": "service:portfolio-service-57116",
    "Name": "Service 'portfolio-service' check",
    "Status": "passing",
    "Notes": "",
    "Output": "HTTP GET http://b543a89da642:57116/health: 200  Output: {\"description\":\"Composite Discovery Client\",\"status\":\"UP\"}",
    "ServiceID": "portfolio-service-57116",
    "ServiceName": "portfolio-service"
  }
]

```
and API check, executed from Docker host:
```sh
curl -s http://localhost:57116/portfolios/customer/1 | jq '.'
```
```JSON
[
  [
    [
      "GE",
      38475
    ],
    [
      "MCD",
      12395
    ],
    [
      "IBM",
      91234
    ]
  ],
  [
    [
      "VZ",
      22342
    ],
    [
      "AXP",
      385432
    ],
    [
      "UTX",
      23432
    ]
  ],
  [
    [
      "IBM",
      18343
    ],
    [
      "DIS",
      45673
    ],
    [
      "AAPL",
      23456
    ]
  ]
]

```
```sh
curl -s http://localhost:57216/pricing/customer/1/portfolio/1 | jq '.'
```

```sh
[
  "VZ 22342 47.050000 1051191.100000",
  "AXP 385432 85.110000 32804117.520000",
  "UTX 23432 110.120000 2580331.840000"
]

```
followed by consul artifact check,  executed from Docker host:
```sh
ROLES='discovery'
for CONTAINER in $ROLES ; do docker container ls | grep $CONTAINER | awk '{print $1}' | xargs -I{} docker exec -t {} /bin/sh -c  'ps -a | grep consu[l]'; done
```
which will show
```sh
 1 root       0:03 /bin/consul agent -config-dir=/config -server -bootstrap -advertise 192.168.99.100
```
```sh
ROLES='discovery'
for CONTAINER in ROLES ; do docker container ls | grep $CONTAINER | awk '{print $1}' | xargs -I{} docker exec -t {} /bin/sh -c 'cat /config/consul.json'; done 
{
	"data_dir": "/data",
	"ui_dir": "/ui",
	"client_addr": "0.0.0.0",
	"ports": {
		"dns": 53
	},
	"recursor": "8.8.8.8",
	"disable_update_check": true
}
```
* forcibly terminate instances
```sh
ROLES=pricing_api
for CONTAINER in $ROLES ; do docker container ls | grep $CONTAINER | awk '{print $1}' | xargs -I{} docker exec -t {} /bin/sh -c 'kill -STOP $(pgrep java)'  ; done
```
followed by observing the service health change in consul ui:
  
NOTE: some project refactoring  performed to get certain Docker is not downloading the images from the parent project docker hub instead of using the local freshly built ones.


Attempt of bootstrapping the cluster via single springboot style command
```sh
export CONSUL_HOST=consul
mvn sprint-boot:run
```
has been failing with host resolution by api CONTAINERs, regardless of consul container was run or not:

```sh
ERROR 18217 --- [  restartedMain] o.s.boot.SpringApplication: Application startup failed
Caused by: java.net.UnknownHostException: consul
```
this was not looked into any further.
### Cleanup

```sh
ROLES='discovery pricing_api portfolio_api'
for CONTAINER in $ROLES  ; do docker container ls | grep $CONTAINER | awk '{print $1}' | xargs -IX docker container stop X; done
for CONTAINER in $ROLES ; do docker container ls -a | grep $CONTAINER | awk '{print $1}' | xargs -IX docker container rm X; done
docker container prune -f
```
```sh
IMAGES='test/pricing test/portfolio'
for IMAGE in $IMAGES ; do docker image ls | grep $IMAGE | awk '{print $3}' | xargs docker image rm {} ; done
docker image prune -f
```
ignoring
```sh
Error response from daemon: invalid reference format	
```
followed by
```sh
echo Y |docker image prune
```

### See also

  * original [Getting Started with Microservices in SpringBoot](https://www.infoq.com/articles/Microservices-SpringBoot) article.
  * [transtation](https://habr.com/company/otus/blog/413567/) (in Russian)
  * [Service Discovery in Microservices](https://www.baeldung.com/cs/service-discovery-microservices)
  * HashiCorp Consul [alternatives/competitors](https://www.g2.com/products/hashicorp-consul/competitors/alternatives)
  * consul [overview](https://xakep.ru/2016/04/18/consul/)(in Russian)
  * consul [intro](https://habr.com/ru/post/266139/)(in Russian)
  * [using consul for service discovery and beyond (in russian)](https://eax.me/consul/)
  * [using consul with sping microservice service discovery](http://cloud.spring.io/spring-cloud-consul/1.3.x/multi/multi_spring-cloud-consul-discovery.html)
  * [Maven plugin for building and pushing Docker images](https://github.com/spotify/docker-maven-plugin)(NOTE: inactive)    
  * [jq intro](https://www.youtube.com/watch?v=NzqBhHVJMDI)
  * [spring-cloud/spring-cloud-consul](https://github.com/spring-cloud/spring-cloud-consul)
  * [core Docker compose example](https://examples.javacodegeeks.com/devops/docker/docker-compose-example/)
    also https://www.javacodegeeks.com/2017/12/microservices-implementation-example-spring-boot.html
  * [Docker maven archetype example](http://geekyplatypus.com/packaging-and-serving-your-java-application-with-docker/)
  * Spring Boot app [consul integration](https://github.com/spring-cloud/spring-cloud-consul) with autoconfiguration and binding.
  * Another [spring-cloud-consul-example](https://github.com/yidongnan/spring-cloud-consul-example) project
  * [Eureka Consul Adapter](https://github.com/twinformatics/eureka-consul-adapter) springboot discovery class.
  * Yet another [spring-boot-starter-consul](https://github.com/markramach/spring-boot-starter-consul) project.
  * docker-compose.yml] [intro](https://habr.com/ru/company/ruvds/blog/486682/) (in Russian)
  * docker-compose.yml] [intro 2](https://habr.com/ru/company/infobox/blog/263001/) (in Russian)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
