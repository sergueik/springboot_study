### Info

Springboot Docker basic project extracted from [springboot mySQL Docker container](https://github.com/TechPrimers/docker-mysql-spring-boot-example) converted too use openjdk jre alpine base image.
### Test

* run locally
```sh
mvn clean spring-boot:run
```
* test locally
```sh
curl http://localhost:8085/basic
Hello basic
```
Copy outside of the project directory
```sh
cp src/main/resources/application.properties ~/Desktop/
```
Modify the setting in the loose property file
``` sh
sed -i 's|8085|8080|' ~/Desktop/application.properties
```

* run in container

```sh
mvn clean package
docker build -f Dockerfile -t basic-example . 
docker run -v ${HOME}/Desktop/:/var/properties -p 8086:8080 basic-example
```

Observe the message:
```sh
s.b.c.e.t.TomcatEmbeddedServletContainer : Tomcat started on port(s): 8080 (http)
```
in the console

test dockerized
```sh
curl http://localhost:8086/basic
Hello basic
```
- need to manually destroy all started containers and image afterwards
```sh
docker contained prune -f
```

### Note 
With passing the parameyters, need to consider
```sh
echo 'a=b c=d e=f g=h'|base64  |base64 -d
```

```sh
a=b c=d e=f g=h
```

### See Also
  * package Springboot as [standalone jar](https://www.baeldung.com/spring-boot-run-maven-vs-executable-jar)
  * move Spring properties File [outside the jar](https://www.baeldung.com/spring-properties-file-outside-jar)
  * parameter-heavy rabbitmq Docker [entrypoint](https://github.com/docker-library/rabbitmq/blob/master/3.8/alpine/docker-entrypoint.sh)