### Info

### Usage
* build application

```sh
mvn package
```
* build container
```sh
docker build -f Dockerfile -t basic-location .
```
* run in container (attached)
```sh
docker run --rm -p 8085:8085 basic-location
```
* test 
```sh
curl -s http://192.168.99.100:8085/check
```
```text
Key file path: /root/.keys/key.txt
Data: 123
```
### NOTE:

* special property `spring.config.location`  allows redefining `application.properties` -not used in this example

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
