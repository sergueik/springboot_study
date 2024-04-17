### Info
  
replica of simple demo for learning
how to monitor spring boot apps via actuator module and prometheus [project](https://github.com/Richard-Yi/springboot-actuator-prometheus-test).
Practice [sample code](https://github.com/akshaythakre/java-microservices-with-spring-cloud-developing-services?ref=https://githubhelp.com)
from [Java Microservices with Spring Cloud: Developing Services](https://app.pluralsight.com/courses/36c015db-2983-4f97-8973-098b6a5d1fcc/table-of-contents)

### Usage

```sh
mvn spring-boot:run
```
```sh
curl -s http://localhost:8080/actuator |jq '.'
```
this will produce
```json
{
  "_links": {
    "self": {
      "href": "http://localhost:8080/actuator",
      "templated": false
    },
    "beans": {
      "href": "http://localhost:8080/actuator/beans",
      "templated": false
    },
    "caches": {
      "href": "http://localhost:8080/actuator/caches",
      "templated": false
    },
    "caches-cache": {
      "href": "http://localhost:8080/actuator/caches/{cache}",
      "templated": true
    },
    "health": {
      "href": "http://localhost:8080/actuator/health",
      "templated": false
    },
    "health-path": {
      "href": "http://localhost:8080/actuator/health/{*path}",
      "templated": true
    },
    "info": {
      "href": "http://localhost:8080/actuator/info",
      "templated": false
    },
    "conditions": {
      "href": "http://localhost:8080/actuator/conditions",
      "templated": false
    },
    "shutdown": {
      "href": "http://localhost:8080/actuator/shutdown",
      "templated": false
    },
    "configprops": {
      "href": "http://localhost:8080/actuator/configprops",
      "templated": false
    },
    "env": {
      "href": "http://localhost:8080/actuator/env",
      "templated": false
    },
    "env-toMatch": {
      "href": "http://localhost:8080/actuator/env/{toMatch}",
      "templated": true
    },
    "loggers": {
      "href": "http://localhost:8080/actuator/loggers",
      "templated": false
    },
    "loggers-name": {
      "href": "http://localhost:8080/actuator/loggers/{name}",
      "templated": true
    },
    "heapdump": {
      "href": "http://localhost:8080/actuator/heapdump",
      "templated": false
    },
    "threaddump": {
      "href": "http://localhost:8080/actuator/threaddump",
      "templated": false
    },
    "prometheus": {
      "href": "http://localhost:8080/actuator/prometheus",
      "templated": false
    },
    "metrics-requiredMetricName": {
      "href": "http://localhost:8080/actuator/metrics/{requiredMetricName}",
      "templated": true
    },
    "metrics": {
      "href": "http://localhost:8080/actuator/metrics",
      "templated": false
    },
    "scheduledtasks": {
      "href": "http://localhost:8080/actuator/scheduledtasks",
      "templated": false
    },
    "mappings": {
      "href": "http://localhost:8080/actuator/mappings",
      "templated": false
    }
  }
}

```
### See Also
   * https://www.baeldung.com/spring-boot-actuators
   * https://baeldung-cn.com/spring-boot-actuator-enable-endpoints
   * [Spring Framework: Spring Boot 2 Actuator](https://app.pluralsight.com/library/courses/cee6a0c4-cc74-43a7-aa39-9dd12971cad9/table-of-contents)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
