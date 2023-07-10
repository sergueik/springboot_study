### Info

### Usage
* build application

```sh
mvn package
```
* pull base image
```sh
docker pull openjdk:8-jre-alpine3.9
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
The application will log
```text
2023-07-10 22:46:26.196  INFO 1 --- [nio-8085-exec-1] example.controller.Worker: Read file from: /root/.keys/key.txt
2023-07-10 22:46:26.236  INFO 1 --- [nio-8085-exec-1] example.controller.Worker: Read  4 bytes
```
and return to `curl`
```text
Data: 123
```
* stop the running container (NOTE: on Windows host, the `^C` does not quite achieve it) and repeat with alternative expession:
```sh
docker run -e LOCATION=../../../../../root/.keys --rm -p 8085:8085 basic-location
```
* test 
```sh
curl -s http://192.168.99.100:8085/check
```
the result will be printed:
```text
Data: 123
```
* repeat with absolute path expession:
```sh
docker run -e LOCATION=/root/.keys --rm -p 8085:8085 basic-location
```
* test 
```sh
curl -s http://192.168.99.100:8085/check
```
```text
Data: 123
```

* repeat with non-existing path:
```sh
docker run -e LOCATION=/tmp/dummy --rm -p 8085:8085 basic-location
```
* test 
```sh
curl -s http://192.168.99.100:8085/check
```
the application will log
```text
2023-07-10 22:53:00.633  INFO 1 --- [nio-8085-exec-1] example.controller.Worker: Read file from: /tmp/dummy/key.txt
2023-07-10 22:53:00.651 ERROR 1 --- [nio-8085-exec-1] example.controller.Worker: Invalid path to the file: /tmp/dummy/key.txt
```
and return to curl the error in the page body:
```text
Data: read error
```
### NOTE:

* special property `spring.config.location` may allow redefining `application.properties` - not used in this example

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
