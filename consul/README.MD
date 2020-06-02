### Info

This directory contains a replica of [consul springboot microservices project](https://github.com/guedim/spring-projects/tree/master/consul-microservice-discovery-sample) used to dissect how exactly `org.springframework.cloud.cient.discovery.DiscoveryClient` does consul service definition and booostrap.

### Testing
 
The cluster is bootstrapped successfully via
step-by-step packaging of both api service applications
```sh
cd portafolio
mvn clean package
docker build -f Dockerfile  -t 'test/portafolio' .
```
```sh
cd pricing
mvn clean package
docker build -f Dockerfile  -t 'test/pricing' .
```
followed by the
```sh
docker-compose -f docker-compose.yaml up
```
which logs to console

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

### Validation
Observed the cluster to be up 
```sh
docker container ls
```
to show
```
CONTAINER ID        IMAGE                    COMMAND                  CREATED             STATUS              PORTS                                                                                                                                NAMES
7233fbe257ae        test/pricing             "sh -c 'java $JAVA_O…"   9 minutes ago       Up 7 minutes        57116/tcp, 0.0.0.0:57216->57216/tcp                                                                                                  consul_pricing_api_1
2d8a3192aa56        test/portafolio          "sh -c 'java $JAVA_O…"   9 minutes ago       Up 7 minutes        0.0.0.0:57116->57116/tcp                                                                                                             consul_portafolio_api_1
11f78b075506        progrium/consul:latest   "/bin/start -server …"   19 hours ago        Up 7 minutes        53/tcp, 0.0.0.0:8300->8300/tcp, 0.0.0.0:8400->8400/tcp, 8301-8302/tcp, 0.0.0.0:8500->8500/tcp, 8301-8302/udp, 0.0.0.0:8600->53/udp   consul_discovery_1
```
connecting to the `consul` container
```sh
docker exec -it $(docker container ls | grep 'consul_discovery' |awk '{print $1}') '/bin/bash'
```
one can find the consul configuration in `/config/consul.json` 
```sh
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
find  /data/services -type f -exec cat {} \;
{"Token":"","Service":{"ID":"pricing-service-57216","Service":"pricing-service","Tags":[],"Address":"7233fbe257ae","Port":57216}}
{"Token":"","Service":{"ID":"portfolio-service-57116","Service":"portfolio-service","Tags":[],"Address":"2d8a3192aa56","Port":57116}}
```

### Additional Testing
followed by consul check
```sh
2>/dev/null curl http://127.0.0.1:8500/v1/health/checks/portfolio-service | jq '.'
```	
```sh
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
and API check:
```sh
curl http://localhost:57116/portfolios/customer/1 | jq '.'
```
```sh
curl http://localhost:57216/pricing/customer/1/portfolio/1 | jq '.'
```

```sh
[
  "VZ 22342 47.050000 1051191.100000",
  "AXP 385432 85.110000 32804117.520000",
  "UTX 23432 110.120000 2580331.840000"
]

```
followed by consul artifact check
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
for CONTAINER in ROLES ; do docker container ls | grep $CONTAINER | awk '{print $1}' | xargs -I{} docker exec -t {} /bin/sh -c  'cat /config/consul.json'; done 
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
```sh
ROLES=pricing_api
for CONTAINER in $ROLES ; do docker container ls | grep $CONTAINER | awk '{print $1}' | xargs -I{} docker exec -t {} /bin/sh -c 'kill -STOP $(pgrep java)'  ; done
```
followed by observing the service health change in consul ui:
  
NOTE: some project refactoring  performed to get certain Docker is not downloading the imagesi
from the parent project docker hub instead of using the local freshly built ones.

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
ROLES='discovery pricing_api portafolio_api'
for CONTAINER in $ROLES  ; do docker container ls | grep $CONTAINER | awk '{print $1}' | xargs -IX docker container stop X; done
for CONTAINER in $ROLES ; do docker container ls -a | grep $CONTAINER | awk '{print $1}' | xargs -IX docker container rm X; done
docker container prune -f
```
```sh
IMAGES='test/pricing test/portafolio'
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
  * [transtation](https://habr.com/company/otus/blog/413567/) (in russian)
  * HashiCorp Consul [alternatives/competitors](https://www.g2.com/products/hashicorp-consul/competitors/alternatives)
  * consul [overview](https://xakep.ru/2016/04/18/consul/)(in Russian)
  * consul [intro](https://habr.com/ru/post/266139/)(in Russian)
  * [using consul for service discovery and beyond (in russian)](https://eax.me/consul/)
  * [using consul with sping microservice service discovery](http://cloud.spring.io/spring-cloud-consul/1.3.x/multi/multi_spring-cloud-consul-discovery.html)
      
  * [jq intro](https://www.youtube.com/watch?v=NzqBhHVJMDI)
  * [spring-cloud/spring-cloud-consul](https://github.com/spring-cloud/spring-cloud-consul)
  * [core Docker compose example](https://examples.javacodegeeks.com/devops/docker/docker-compose-example/)
    also https://www.javacodegeeks.com/2017/12/microservices-implementation-example-spring-boot.html
  * [Docker maven archetype example](http://geekyplatypus.com/packaging-and-serving-your-java-application-with-docker/)
  * Spring Boot app [consul integration](https://github.com/spring-cloud/spring-cloud-consul) with autoconfiguration and binding.
  * Another [spring-cloud-consul-example](https://github.com/yidongnan/spring-cloud-consul-example) project
  * [Eureka Consul Adapter](https://github.com/twinformatics/eureka-consul-adapter) springboot discovery class.
  * Yet another [spring-boot-starter-consul](https://github.com/markramach/spring-boot-starter-consul) project.
  * docker-compse.yml] [intro](https://habr.com/ru/company/ruvds/blog/486682/) (in Russian)
  * docker-compse.yml] [intro 2](https://habr.com/ru/company/infobox/blog/263001/) (in Russian)
