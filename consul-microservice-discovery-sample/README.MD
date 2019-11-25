### Info

This directory contains a replica of [consul springboot microservices project](https://github.com/guedim/spring-projects/tree/master/consul-microservice-discovery-sample)
### Testing

The cluster is bootstrapped successfully via 
step-by-step packaging of both api service applications 
```sh
cd portofolio
mvn clean package
``` 
```sh
cd pricing
mvn clean package
``` 
followed by the 
```sh
docker-componse -f docker-compose.yml up
```
followed by consul check
```
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
NOTE: it is unclear if docker is downloading the images 
from the hub insteat of using the local freshly built ones.

Running the cluster via 
```sh
mvn sprint-boot:run
```
has been failing with host resolution by api nodes:
```
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
