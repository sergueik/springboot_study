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
curl -s -X POST -H "Content-Type: application/json" -d '{"id":"1", "name": "Adam" }' http://localhost:8080/
curl -s -X POST -H "Content-Type: application/json" -d '{"id":"2", "name": "Eve" }' http://localhost:8080/
```
```sh
curl -s http://localhost:8080/person
```
```JSON
[{"id":"1","name":"Adam"},{"id":"2","name":"Eva"},{"id":"643dc2714d093de9589b0553","name":"Paul"}]

```
```sh
curl -s http://localhost:8080/person/name/A
```
```JSON
[{"id":"1","name":"Adam"}]
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
