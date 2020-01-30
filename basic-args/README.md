### Info

Springboot Docker basic argument exercise project cloned from
[lordoftheflies/example-spring-boot-with-command-line-arguments](https://github.com/lordoftheflies/example-spring-boot-with-command-line-arguments)

### Basic

#### Run Locally

```sh
mvn clean spring-boot:run -Dappname="my application"
```
* test locally
```sh
curl http://localhost:8080/basic
This is my application
```

#### Run on container

```sh
mvn -Dappname="my application" test
mvn -Dmaven.test.skip=true clean package
docker build -f Dockerfile -t basic-args-example . 
docker run -p 8080:8080 basic-args-example
```

Observe the message:
```sh
INFO 1 --- [           main] 
example.AppCommandLineConfiguration      : Loaded with appname: my docker appication```
in the console

test dockerized
```sh
curl http://localhost:8080/basic
This is my docker application
```
- need to manually destroy all started containers and image afterwards
```sh
docker contained prune -f
```
### Note
However this does not scale - every injected paramter must be provided, and with the growing number of parameters this goes out of control
or else
```sh
mvn clean spring-boot:run
```
throws the 
```sh
InvocationTargetException: Error creating bean with name 'exampleApplication': 
Injection of autowired dependencies failed; nested exception is 
java.lang.IllegalArgumentException: Could not resolve placeholder 'appname' in value "${appname}"

### Complex Parameters
```
mvn clean spring-boot:run -DDATA=eyJuYW1lIjoidmFsdWUiLCAic3VjY2VzcyI6dHJ1ZSwicmVzdWx0Ijo0MiwiaWQiOjAgfQo=

```

### See Also
   * [tutorial](https://howtodoinjava.com/spring-boot2/application-arguments/) for dealing with the application runtime arguments in a `@Component`
   * discussion of [shell entrypoints](https://stackoverflow.com/questions/37904682/how-do-i-use-docker-environment-variable-in-entrypoint-array) passing in environments
