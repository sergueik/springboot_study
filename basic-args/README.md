### Info

Springboot Docker basic argument exercise project cloned from
[lordoftheflies/example-spring-boot-with-command-line-arguments](https://github.com/lordoftheflies/example-spring-boot-with-command-line-arguments)
Modified to enable passing an arbirtary data structure wrapped in json and subsequntly base 64 encoded, through  SpringBoot
eliminating modifying low level details of the stack delivery engine when there is a business application signature change

### Basic

#### Pack parameters
```sh
echo '{"name":"my parameterized spring application", "success":true,"result":42,"id":0 }' | base64
```
```sh
eyJuYW1lIjoibXkgcGFyYW1ldGVyaXplZCBzcHJpbmcgYXBwbGljYXRpb24iLCAic3VjY2VzcyI6dHJ1ZSwicmVzdWx0Ijo0MiwiaWQiOjAgfQo=
```

#### Run Locally


```sh
mvn clean \
 -Dparams=eyJuYW1lIjoibXkgcGFyYW1ldGVyaXplZCBzcHJpbmcgYXBwbGljYXRpb24iLCAic3VjY2VzcyI6dHJ1ZSwicmVzdWx0Ijo0MiwiaWQiOjAgfQo= \
 spring-boot:run
```
* test locally
```sh
curl http://localhost:8080/basic
This is my parameterized spring application
```

```sh
mvn -Dmaven.test.skip=true clean package
java -Dparams=eyJuYW1lIjoibXkgcGFyYW1ldGVyaXplZCBzcHJpbmcgYXBwbGljYXRpb24iLCAic3VjY2VzcyI6dHJ1ZSwicmVzdWx0Ijo0MiwiaWQiOjAgfQo= \
 -jar target/example.basic-args.jar
```
#### Run on container
Pass in dummy params during the test
```sh
echo '{"id":0}' | base64
```
```sh
eyJpZCI6MH0K
```
```sh
mvn -Dparams=eyJpZCI6MH0K test
mvn -Dmaven.test.skip=true clean package
docker build -f Dockerfile -t basic-args-example .
docker run -p 8080:8080 basic-args-example
```

test dockerized
```sh
curl http://localhost:8080/basic
This is my docker application
```
- may like to destroy all started containers and images afterwards
```sh
docker contained prune -f
```

### See Also
   * [tutorial](https://howtodoinjava.com/spring-boot2/application-arguments/) for dealing with the application runtime arguments in a `@Component`
   * discussion of [shell entrypoints](https://stackoverflow.com/questions/37904682/how-do-i-use-docker-environment-variable-in-entrypoint-array) passing in environments
   * spring boot application [dealing with](https://www.logicbig.com/tutorials/spring-framework/spring-boot/app-args.html) application arguments