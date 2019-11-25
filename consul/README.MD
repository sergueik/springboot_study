### Info

This directory contains a replica of [consul springboot microservices project](https://github.com/guedim/spring-projects/tree/master/consul-microservice-discovery-sample)
### Testing

The cluster is bootstrapped successfully via
step-by-step packaging of both api service applications
```sh
cd portofolio
mvn clean package
docker build -f Dockerfile  -t 'test/portofolio' .
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
