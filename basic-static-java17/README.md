### Info

Basic Springboot application hosting static page rendered by thymeleaf spring framework with a Angular JS and Bootstrap loaded, and few static resources loaded
with unit tests on HTTP status and page content validations (partially working) added

### Testing

```sh
docker pull openjdk:17-alpine
```

* compile and package jar on JDK17/Maven "builder" container and copy the jar into a searate Docker image with JRE 17:

```sh
DOCKER_IMAGE=alpine-jdk17-maven
docker build -t $DOCKER_IMAGE -f Dockerfile.$DOCKER_IMAGE .
docker run --rm -v $(echo $HOME)/maven/.m2:/tmp/maven/.m2 -v $(pwd):/app --entrypoint sh -p 8080:8080 -it $DOCKER_IMAGE
```

in the container run

```sh
mvn spring-boot:run
```

try to open static page in the browser

![Docker 404](https://github.com/sergueik/springboot_study/blob/master/basic-static-java17/screenshots/capture404.png)
will give 404
and the container log will have information:

```text
2024-04-23T22:09:55.958Z  WARN 56 --- [nio-8080-exec-2] o.s.web.servlet.PageNotFound             : No mapping for GET /home.html
```

alternatively
```sh
curl http://localhost:8080/home.html
```

```text
{"timestamp":1713910195959,"status":404,"error":"Not Found","path":"/home.html"}
```
* NOTE: the broken version is still useful with older releases of Java.

* fixed version
```sh
curl http://localhost:8080/home.html
```
gives 
```HTML
<!DOCTYPE html>
<html>
    <head>
        <meta charset="UTF-8">
    </head>
    <body>
        It is home page
    </body>
</html>
```

also supported the templates `http://localhost:8080/upload`

* NOTE:the config class may need to be reviewed in Java 11 case)
* test locally (checkout the hash `a5c326c9db97d6852b7d61798afdfceae1269b02` to restore the older version)

```sh
git checkout a5c326c9db97d6852b7d61798afdfceae1269b02
```
```sh
mvn -f pom.JAVA11.xml spring-boot:run
```
```sh
curl http://localhost:8080/home.html
```
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
