replica of reactive Spring Demo with Webflux and R2DBC and MVNW [project](https://github.com/neil-writes-code/reactive-spring-demo) switched to Docker images
and no-graalVM application packaging
### Usage

```sh
docker pull eclipse-temurin:17-jdk-alpine  
docker pull eclipse-temurin:17-jre-alpine  
docker pull maven:3.8.3-openjdk-17
docker pull vegardit/graalvm-maven:latest-java17
docker pull postgres:9.6-alpine3.13
```
```sh
docker image ls
```
```text
vegardit/graalvm-maven   latest-java17       035b44e43e23        3 days ago          1.01GB
eclipse-temurin          17-jre-alpine       0aa0f849b44d        6 weeks ago         181MB
eclipse-temurin          17-jdk-alpine       1d00d209bc23        6 weeks ago         333MB
postgres                 9.6-alpine3.13      60d950d0004d        4 years ago         37.2MB

```

```sh
ls -hl ~/.docker/machine/machines/default/disk.vmdk
```
```text
-rw-r--r-- 1 kouzm 197609 6.5G Dec 20 15:56 disk.vmdk
```
```sh
docker-compose -f docker-compose.mvn.yml up --build --detach
```
```
curl -sH "Content-Type: application/json" http://192.168.99.100:8080/departments  | jq
```
```JSON
[
  {
    "id": 1,
    "name": "Software Development",
    "manager": {
      "id": 1,
      "firstName": "Bob",
      "lastName": "Steeves",
      "position": "Director of Software Development",
      "fullTime": true
    },
    "employees": [
      {
        "id": 2,
        "firstName": "Neil",
        "lastName": "White",
        "position": "Software Developer",
        "fullTime": true
      },
      {
        "id": 3,
        "firstName": "Joanna",
        "lastName": "Bernier",
        "position": "Software Tester",
        "fullTime": false
      }
    ]
  },
  {
    "id": 2,
    "name": "HR",
    "manager": {
      "id": 4,
      "firstName": "Cathy",
      "lastName": "Ouellette",
      "position": "Director of Human Resources",
      "fullTime": true
    },
    "employees": [
      {
        "id": 5,
        "firstName": "Alysha",
        "lastName": "Rogers",
        "position": "Intraday Analyst",
        "fullTime": true
      }
    ]
  }
]

```
```sh
docker-compose -f docker-compose.vanilla.yml stop
```
```sh
docker-compose -f docker-compose.vanilla.yml rm -f

```
```sh
docker system prune -f
docker image rm eclipse-temurin:17-jre-alpine
```
#### Troubleshooting



```text
#0 161.4 [ERROR] Failed to execute goal org.apache.maven.plugins:maven-dependency-plugin:3.3.0:go-offline (default-cli) on project hr-service: org.eclipse.aether.resolution.DependencyResolutionException: Failed to read artifact descriptor for org.springframework.experimental:spring-aot-maven-plugin:jar:0.12.1: org.springframework.experimental:spring-aot-maven-plugin:pom:0.12.1 failed to transfer from https://repo.spring.io/release during a previous attempt. This failure was cached in the local repository and resolution is not reattempted until the update interval of spring-releases has elapsed or updates are forced. Original error: Could not transfer artifact org.springframework.experimental:spring-aot-maven-plugin:pom:0.12.1 from/to spring-releases (https://repo.spring.io/release): authentication failed for https://repo.spring.io/release/org/springframework/experimental/spring-aot-maven-plugin/0.12.1/spring-aot-maven-plugin-0.12.1.pom, status: 401 Unauthorized -> [Help 1]
```
replace the url in `pom.xml` `pluginRepository` and `repository`[stackoverflow](https://stackoverflow.com/questions/76132503/spring-aot-maven-plugin-0-11-3-pom-status-401-unauthorized):
```xml
    <pluginRepository>
      <id>spring-releases</id>
      <name>Spring Releases</name>
      <url>https://repo.spring.io/milestone</url>
      <snapshots>
        <enabled>false</enabled>
      </snapshots>
    </pluginRepository>

```
To troubleshoot
```text
#0 48.12 [ERROR] Failed to execute goal org.apache.maven.plugins:maven-resources-plugin:3.2.0:resources (default-resources) on project hr-service: Cannot create resource output directory: /app/target/classes -> [Help 1]
```

```text
#0 203.2 [ERROR] Failed to execute goal org.graalvm.buildtools:native-maven-plugin:0.9.13:build (build-native) on project hr-service: 'gu' tool wasn't found. This probably means that JDK at isn't a GraalVM distribution. -> [Help 1]

```
### See Also

  * [benchmarking](https://habr.com/ru/companies/domclick/articles/970104/)(in Russian)


