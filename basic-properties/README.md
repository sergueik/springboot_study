### Info

Springboot Docker basic project based on [springboot mySQL Docker container](https://github.com/TechPrimers/docker-mysql-spring-boot-example)
converted to pass Spring `application.properties` file configuration separately into the container
hosted application via entrypoint.
Later modified to include ReloadableProperties class sample from [collection of small and focused Spring tutorials](https://github.com/eugenp/tutorials/tree/master/spring-boot-modules/spring-boot-properties/src/main/java/com/baeldung/properties/reloading) demonstrating `application.properties` reloading feature.

### Test

#### Local execution
Note: - after turning on trimmed down `value.property` magic the default command no longer works
```sh
mvn clean spring-boot:run
```
followed by

the hard coded health checks
```sh
curl http://localhost:8085/basic
```
will respond with 
```sh
Hello basic
```
but
```sh
ttp://localhost:8085/worker
```
will print
```sh
Hello null
```
The argument-less build and run via maven plugin command does not fail,
but apprently is not loading the `application.properties`, passing the argument 
via define solves the issue:
```sh
mvn -Dspring.config.location=src/main/resources/application.properties spring-boot:run
```
now
```sh
curl http://localhost:8085/worker
```
returns
```sh
Hello some value
```


and updates instantly when `application.properties` is changed.
It is also possible to pass the absolute path outside the project dir:
```sh
mvn -Dspring.config.location=$(pwd)/src/main/resources/application.properties spring-boot:run
```

one can place properties ouside of the project dir pass full path:

```sh
cp src/main/resources/application.properties /tmp
sed -i 's|\(application.property\)=.*$|\1=different value|' /tmp/application.properties
grep -E 'application.property' '/tmp/application.properties'
mvn -Dspring.config.location=/tmp/application.properties spring-boot:run
```
One can also use `file://` prefix with no harm.
```sh
mvn -Dspring.config.location=file:///tmp/application.properties spring-boot:run
```
followed by validation
```sh
curl -vk 192.168.0.64:8085/worker
```
on Windows there appears to be a problem 
```powershell
out-File -LiteralPath C:\temp\application.properties -NoNewline -InputObject ([system.String]::Join("`n", (get-content -path .\src\main\resources\application.properties)).replace('some value','different value'))
```
both
```powershell
mvn -Dspring.config.location=/temp/application.properties spring-boot:run
```
```
mvn -Dspring.config.location=c:/temp/application.properties spring-boot:run
```
result in
```sh
curl http://localhost:8085/worker
```
returning 
```sh
Hello unknown
```
indicating that property file was not read.

note that one has to remove the drive letter from the path (assuming the configuration properfies file is on the same drive)
```sh

mvn -Dspring.config.location=file:///temp/application.properties spring-boot:run
```
(in the absence of `file://` prefix the relative `application.properties` path will be attempted)
```sh
```
so proceed to the next step of dockerizing the app.

#### Run Jar Locally 
```sh
mvn package
```
```sh
cp src/main/resources/application.properties ~/Desktop/
```
```sh
java -jar target/example.basic-properties.jar --spring.config.location=file:///home/$(whoami)/Desktop/application.properties
```
* test locally
```sh
curl http://localhost:8085/worker
Hello some value
```
* change property
```sh
sed -i 's|some value|some other value|' ~/Desktop/application.properties
```
* observe application reload being logged:
```sh
...
o.a.c.c.PropertiesConfiguration: 
Reloading configuration. 
URL is file:/home/sergueik/Desktop/application.properties
```
* verify
```sh
curl http://localhost:8085/worker
Hello some other value
```
#### Dockerized App Tests

* copy the properties file outside of the project directory (the one packed in the jar will be ignored)
```sh
cp src/main/resources/application.properties ~/Desktop/
```
* modify the setting in the loose property file changing port to be certain to interact with the application running in the container
```sh 
sed -i 's|8085|8080|' ~/Desktop/application.properties
```
* make sure the `ENTRYPOINT` in the `Dockerfile` has
the `--spring.config.location` java option set 
to pass the properties file location of the mapped directory path to the Spring app:
```
"java", "-jar", "app.jar", "--spring.config.location=file:///var/properties/application.properties"
```
* build container

```sh
mvn clean package
docker build -f Dockerfile -t basic-example .
```
* run in container (attached)
```sh
docker run -v ${HOME}/Desktop/:/var/properties -p 8086:8080 basic-example
```
* test via curl
```sh
curl http://localhost:8086/worker
Hello some value
```
* modify property file once again
```sh
sed -i 's|\(value.property\)=.*$|\1=new value|' ~/Desktop/application.properties
```
* confirm to reflect
```sh
curl http://localhost:8086/worker
Hello new value
```
* observe the message in Docker console:
```sh
o.a.c.c.PropertiesConfiguration: 
Reloading configuration. URL is file:/var/properties/application.properties
```

#### Cleanup
* may need to manually destroy all started containers and images
```sh
docker container prune -f
docker image rm basic-example
docker image prune -f
```

### See Also
  * package Springboot as [standalone jar](https://www.baeldung.com/spring-boot-run-maven-vs-executable-jar)
  * move Spring properties File [outside the jar](https://www.baeldung.com/spring-properties-file-outside-jar)
  * parameter-heavy rabbitmq Docker [entrypoint](https://github.com/docker-library/rabbitmq/blob/master/3.8/alpine/docker-entrypoint.sh)
  * auto-reloading the Spring application when a proferty file change detected via `PropertiesConfiguration` that periodically checks for the [file modifications](https://www.baeldung.com/spring-reloading-properties) every poll interval
  * как перечитывать настройки после изменения properties файла [(in Russian](https://qna.habr.com/q/713981)
  * overriding SptingBoot `application.properties` during [test run](https://fooobar.com/questions/85477/override-default-spring-boot-applicationproperties-settings-in-junit-test) (in Russian)
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
