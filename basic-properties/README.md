### Info

Springboot Docker basic project based on [springboot mySQL Docker container](https://github.com/TechPrimers/docker-mysql-spring-boot-example) converted to pass Spring `application.properties` file configuration separately into the containerhosted application via entrypoint. Later modified to demonstrate reloadable `application.properties` feature.

### Test

* run locally - may no longer work after turning on trimmed down `application.property` magic
```sh
mvn clean spring-boot:run
```
will not fail, but unlikely to be loading the `application.properties`, so proceed to the next step.
* run locally as jar
```sh
mvn package
```
```sh
cp src/main/resources/application.properties ~/Desktop/
```
```sh
java -jar target/example.basic-properties.jar --spring.config.location=file:///home/sergueik/Desktop/application.properties
```
* test locally
```sh
curl http://localhost:8085/basic
Hello some value
```
* change property
```sh
sed -i 's|some value|some other value|' ~/Desktop/application.properties
```
observe application reload being logged:
```sh
o.a.c.c.PropertiesConfiguration          : Reloading configuration. URL is file:/home/sergueik/Desktop/application.properties
```
verify
```sh
curl http://localhost:8085/basic
Hello some other value
```

* copy the properties file outside of the project directory (the one packed in the jar will be ignored)
```sh
cp src/main/resources/application.properties ~/Desktop/
```
* modify the setting in the loose property file
change port
``` sh
sed -i 's|8085|8080|' ~/Desktop/application.properties
sed -i 's|some value|yet another value|' ~/Desktop/application.properties
```
and
MAke sure the `ENTRYPOINT` uses  java option to pass the propertie file locatio n to the spring app:
```
"java", "-jar", "app.jar", "--spring.config.location=file:///var/properties/application.properties"

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
Hello yet another value
```
modify once again.
```sh
sed -i 's|\(application.property\)=.*$|\1=new value|' ~/Desktop/application.properties
```
confirm to reflect:
```sh
curl http://localhost:8086/basic
Hello new value
```
- need to manually destroy all started containers and image afterwards
```sh
docker contained prune -f
```

### See Also
  * package Springboot as [standalone jar](https://www.baeldung.com/spring-boot-run-maven-vs-executable-jar)
  * move Spring properties File [outside the jar](https://www.baeldung.com/spring-properties-file-outside-jar)
  * parameter-heavy rabbitmq Docker [entrypoint](https://github.com/docker-library/rabbitmq/blob/master/3.8/alpine/docker-entrypoint.sh)
  * auto-reloading the Spring application when a proferty file change detected via `PropertiesConfiguration` that checks for the [file modification date](https://www.baeldung.com/spring-reloading-properties) every poll interval
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
