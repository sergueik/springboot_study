### Info

This directory contains the [example](https://stackoverflow.com/questions/2712970/get-maven-artifact-version-at-runtime) of getting Maven artifact version at runtime for a basic Spring-boot ap

### Usage
```sh
mvn spring-boot:run
```
this prints to console:
```text
Version:
reading the resource
failed to find the version
Version:

  .   ____          _            __ _ _
 /\\ / ___'_ __ _ _(_)_ __  __ _ \ \ \ \
( ( )\___ | '_ | '_| | '_ \/ _` | \ \ \ \
 \\/  ___)| |_)| | | | | || (_| |  ) ) ) )
  '  |____| .__|_| |_|_| |_\__, | / / / /
 =========|_|==============|___/=/_/_/_/
 :: Spring Boot ::        (v2.3.4.RELEASE)
```
```sh
mvn package
java -jar target\example.banner.jar
```
```text
Version: 0.4.0-SNAPSHOT
reading the resource: maven/example/banner/pom.properties
read the resource: maven/example/banner/pom.properties
read the version: 0.4.0-SNAPSHOT
reading the resource: MANIFEST.MF
read the resource: MANIFEST.MF
read the version: 0.4.0-SNAPSHOT
Version: 0.4.0-SNAPSHOT
  .   ____          _            __ _ _
 /\\ / ___'_ __ _ _(_)_ __  __ _ \ \ \ \
( ( )\___ | '_ | '_| | '_ \/ _` | \ \ \ \
 \\/  ___)| |_)| | | | | || (_| |  ) ) ) )
  '  |____| .__|_| |_|_| |_\__, | / / / /
 =========|_|==============|___/=/_/_/_/
 :: Spring Boot ::        (v2.3.4.RELEASE)
```

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
