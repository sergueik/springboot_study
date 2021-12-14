### Info

This directory contains code fragement based on example custom health indicator tutorial [health Indicators in Spring Boot](https://www.baeldung.com/spring-boot-health-indicators)

### Usage
```sh
mvn spring-boot:run
```
then repeatedly checking the default health URI
```sh
curl http://localhost:8080/actuator/health/random |jq '.'
```
eventally gets a random `value` over 0.9 and return the `DOWN` status:
```json
"heartbeatControllerExample": {
  "status": "DOWN",
  "details": {
	"error": "value: 0.92",
	"timestamp": "Jan 05,2021 17:31"
  }
}
```
### See Also
  * https://stackoverflow.com/questions/47935369/spring-boot-healthindicator-by-example
  * https://reflectoring.io/spring-boot-health-check/
  * https://github.com/thombergs/code-examples/tree/master/spring-boot/spring-boot-health-check
  * https://reflectoring.io/spring-boot-health-check/

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
