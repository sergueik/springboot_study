### Info

This directory contains a replica of [consul springboot microservices project](https://github.com/guedim/spring-projects/tree/master/consul-microservice-discovery-sample)
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
docker-componse -f docker-compose.yml up
```
which logs to console

```sh
sergueik@sergueik71:~/src/springboot_study/consul$ docker-compose -f docker-compose.yml  up
Creating network "consul_default" with the default driver
Creating consul_consul_1 ... done
Creating consul_portafolio_test_1 ... done
Creating consul_pricing_test_1    ... done
Attaching to consul_consul_1, consul_portafolio_test_1, consul_pricing_test_1
consul_1           | ==> WARNING: Bootstrap mode enabled! Do not enable unless necessary
consul_1           | ==> WARNING: It is highly recommended to set GOMAXPROCS higher than 1
consul_1           | ==> Starting raft data migration...
consul_1           | ==> Starting Consul agent...
consul_1           | ==> Starting Consul agent RPC...
consul_1           | ==> Consul agent running!

```
followed with the massive burst of usual Spring messages
followed by consul check
```sh
2>/dev/null curl http://127.0.0.1:8500/v1/health/checks/portfolio-service | jq '.'
```	
```sh
[
  {
    "Node": "127.0.0.1.xip.io",
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
for N in 'consul_consul' ; do docker container ls | grep $N | awk '{print $1}' | xargs -I{} docker exec -t {} /bin/sh -c  'ps -a | grep consu[l]'; done
```
```sh
 1 root       0:03 /bin/consul agent -config-dir=/config -server -bootstrap -advertise 192.168.99.100
```
```sh
for N in 'consul_consul' ; do docker container ls | grep $N | awk '{print $1}' | xargs -I{} docker exec -t {} /bin/sh -c  'cat /config/consul.json'; done 
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
for N in 'test/portafolio' ; do docker container ls | grep $N | awk '{print $1}' | xargs -I{} docker exec -t {} /bin/sh -c 'kill -STOP $(pgrep java)'  ; done
```
followed by observing the service health change in consul ui:
  
NOTE: some project refactoring  performed to get certain Docker is not downloading the images
from the parent project docker hub instead of using the local freshly built ones.

Attempt of bootstrapping the cluster via single springboot style command
```sh
export CONSUL_HOST=consul
mvn sprint-boot:run
```
has been failing with host resolution by api nodes, regardless of consul container was run or not:

```sh
ERROR 18217 --- [  restartedMain] o.s.boot.SpringApplication: Application startup failed
Caused by: java.net.UnknownHostException: consul
```
this was not looked into any further.
### Cleanup

```sh
for N in  consul 'test/pricing' 'test/portafolio' ; do docker container ls | grep $N | awk '{print $1}' | xargs -IX docker container stop X; done
for N in  consul 'test/pricing' 'test/portafolio' ; do docker container ls -a | grep $N | awk '{print $1}' | xargs -IX docker container rm X; done

```
```sh
for I in 'test/pricing' 'test/portafolio' ; do docker image ls | grep $I | awk '{print $3}' | xargs  docker image rm {} ; done
```
ignoring
```sh
Error response from daemon: invalid reference format	
```
### See also

  * original [Getting Started with Microservices in SpringBoot](https://www.infoq.com/articles/Microservices-SpringBoot) article.
  * [Russian transtation (in russian)](https://habr.com/company/otus/blog/413567/)
  * [using consul for service discovery and beyond (in russian)](https://eax.me/consul/)
  * [using consul with sping microservice service discovery](http://cloud.spring.io/spring-cloud-consul/1.3.x/multi/multi_spring-cloud-consul-discovery.html)
  * [jq intro](https://www.youtube.com/watch?v=NzqBhHVJMDI)
  * [spring-cloud/spring-cloud-consul](https://github.com/spring-cloud/spring-cloud-consul)
  * [core Docker compose example](https://examples.javacodegeeks.com/devops/docker/docker-compose-example/)
  * [Docker maven archetype example](http://geekyplatypus.com/packaging-and-serving-your-java-application-with-docker/)
