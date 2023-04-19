### Info
this directory contains a replica of __spring data with MongoDB repository with spring data generated query insert update and delete methods example__ [repository](https://github.com/upanshu21/springboot-mongodb-crud)

### Usage
### Unix


#### Windows Host with Docker Toolbox

```sh
docker pull mvertes/alpine-mongo
docker-compose up --build
```
```sh
IMAGE=
docker exec -it $IMAGE sh
```
```sh
MONGO_HOST=$(docker-machine ip)
```
or manually
```cmd
docker-machine ip
set MONGO_HOST=192.168.99.100
```
followed with
```sh
mvn test
```
and
```cmd
mvn spring-boot:run
```
then in separate console:
```sh
curl -s -X POST -H "Content-Type: application/json" -d '{"id":1, "name": "Adam" }' http://localhost:8080/person
curl -s -X POST -H "Content-Type: application/json" -d '{"id":2, "name": "Eve" }' http://localhost:8080/person
```
```sh
curl -s http://localhost:8080/person
```
```JSON
[{"id":1,"name":"Adam","tickets":[]},{"id":2,"name":"Eva","tickets":[]}]
```

```sh
curl -s -X PUT -H "Content-Type: application/json" -d '[{"appId":1, "status": "new" }]' http://localhost:8080/person/addticket/1
```
```sh
curl -s http://localhost:8080/person/1
```
```JSON
{"id":1,"name":"Adam","tickets":[{"id":null,"appId":"1","status":"new"}]}
```
NOTE: only when added explicitly, `Ticket` has id:
```sh
curl -s -X POST -H "Content-Type: application/json" -d '{"appId":10, "status": "unsure" }' http://localhost:8080/person/ticket
```
```JSON
{"id":"64405f40426a552864e3c56c","appId":"10","status":"unsure"}
```
NOTE: When the `Person` class has `Optional<List<Ticket>>` `tickets` property, query will raise the exception:
```text
java.lang.UnsupportedOperationException: Cannot set immutable property java.util.Optional.value!] with root cause

java.lang.UnsupportedOperationException: Cannot set immutable property java.util.Optional.value!
        at org.springframework.data.mapping.model.BeanWrapper.setProperty(BeanWrapper.java:87) ~[spring-data-commons-2.3.4.RELEASE.jar:2.3.4.RELEASE]
...
...
at com.sun.proxy.$Proxy63.findById(Unknown Source) ~[na:na]
```
and when it is declared to be List<Ticket> the query will raise the exception:
```text
org.springframework.data.mapping.model.MappingInstantiationException:
Failed to instantiate java.util.List using constructor NO_CONSTRUCTOR with arguments ] with root cause
org.springframework.beans.BeanInstantiationException:
Failed to instantiate[java.util.List]:Specified class is an interface
```
```sh
curl -s http://localhost:8080/person/greater/2
```
```JSON
[{"id":2,"name":"Eve","tickets":[]}]
```
* if need to run the query directly then
```sh
IMAGE=mongo-service
docker exec -it $IMAGE sh
```
in the shell
```sh
mongo
```
in mongo shell
```sh
show databases
```
```text
admin              0.000GB
config             0.000GB
local              0.000GB
spring-mongo-crud  0.000GB
```
```sh
use spring-mongo-crud
```

```sh
show tables
```
```text
Person
```
```
db.Person.find({"name": "Adam"})
```
```JSON
{ "_id" : "1", "name" : "Adam", "_class" : "example.model.Person" }
```
### See Also

  * [pluralsight training](https://app.pluralsight.com/courses/84e60231-ee35-45ff-a83e-1f3e17f54206/table-of-contents) on __Spring Framework: Spring Data MongoDB 2__
