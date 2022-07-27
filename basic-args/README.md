### Info

Springboot Docker basic argument exercise project based on
[lordoftheflies/example-spring-boot-with-command-line-arguments](https://github.com/lordoftheflies/example-spring-boot-with-command-line-arguments)
Modified to pass an arbirtary data structure wrapped in json and subsequntly base 64 encoded, through  SpringBoot - hopefully
eliminating modifying low level details of the stack delivery engine when there is a business application signature change

### Basic
```sh
TAG=echo
docker build -t $TAG -f Dockerfile.$TAG .
```
then
```sh
docker run -it $TAG
```
will respond with
```sh
hello world
```
and
```sh
docker run -it $TAG john
```
will respond with
```sh
hello john
```
and
```sh
docker run -it $TAG john paul ringo george
```
will respond with
```sh
hello john paul ringo george
```
alternatively use Docker `CMD` / `ENTRYPOINT` interplay:
```sh
TAG=default_arg
docker build -t $TAG -f Dockerfile.$TAG .
docker run -it $TAG message1 message2 message3
```
will respond with
```sh
Hello message1 message2 message3!
```
and
```sh
docker run -it $TAG 
```
will respond with
```sh
hello world
```
hoewever, this does not work right with multiple arguments:
```sh
docker run -it $TAG  message1 message2
```
will respond with
```sh
Hello message1!
Hello message2!
```
#### Note
To pass parameters a small shell script is created in the Docker image to workaround was created to handle the `$0` which receives the argument of the command `docker` is run, but when none provided, gets the value of the `sh`:
```sh
if [ "$1" == "sh" ]
then
  VAR=world
else
  VAR="$@"
  VAR=${VAR:-world}
fi
echo "hello $VAR"
```
One then can call it in one of the following ways:
```sh
ENTRYPOINT [ "/tmp/a.sh" ]
```
or
```sh
ENTRYPOINT ["sh", "-c", "/tmp/a.sh \"$0\" \"$@\"" ]
```
A simplified  version 
```
#!/bin/sh
echo hello ${1:-world}
```
that is a de-facto standard of variable substitution
does not appear to work because of the argument 0 being used when performing Docker `run`.

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
```
You will notice the console to show
```
Processing payload:
{
  "name":"my parameterized spring application",
  "success":true,
  "result":42,
  "id":0
}

Accepted keys: (?:id|name|success|result)

```
followed by
```sh

Processing key: result
Loaded string: result: 42
Processing key: success
Loaded string: success: true
Processing key: name
Loaded string: name: my parameterized spring application
Processing key: id
Loaded string: id: 0

```
and the `Application` class processing the `GET` request

```java
@Autowired
// not exposed about that params is linked to ApplicationArguments
private Params params;

@GetMapping
public String Hello() {
	final String appname = params.getAppname();
	final int result = params.getResult();
	return "This is " + appname + " and the result is: " + result;
}
```
now already has its specific `appName`, `result`
and whatelse internally passed into it via the base 64 encoded JSON  of `params` commandline argument:
```sh
This is my parameterized spring application and the result is: 42
```
Alternarively one can quit reelying on Maven life cycles and run it straight like a jar with the same effect:

```sh
mvn -Dmaven.test.skip=true clean package
java -Dparams=eyJuYW1lIjoibXkgcGFyYW1ldGVyaXplZCBzcHJpbmcgYXBwbGljYXRpb24iLCAic3VjY2VzcyI6dHJ1ZSwicmVzdWx0Ijo0MiwiaWQiOjAgfQo= \
 -jar target/example.basic-args.jar
```

This will be useful when dockerizing the same - eliminating the need to tune 'deployment' details after business requirement change.

#### Run on container
Package the jar (NOTE: one can pass in dummy params during the test)
```sh
echo '{"id":0}' | base64
```
```sh
eyJpZCI6MH0K
```
```sh
mvn -Dparams=eyJpZCI6MH0K test
mvn -Dmaven.test.skip=true clean package
```
modify params
```sh
echo '{"name":"my dockerized hparameterized spring application", "success":true,"result":42,"id":0 }' | base64 | tr -d '\n' | tee /tmp/params.$$
```

```sh
eyJuYW1lIjoibXkgZG9ja2VyaXplZCBocGFyYW1ldGVyaXplZCBzcHJpbmcgYXBwbGljYXRpb24iLCAic3VjY2VzcyI6dHJ1ZSwicmVzdWx0Ijo0MiwiaWQiOjAgfQo=
```
```sh
docker build -f Dockerfile -t basic-args-example --build-arg "params=eyJuYW1lIjoibXkgZG9ja2VyaXplZCBocGFyYW1ldGVyaXplZCBzcHJpbmcgYXBwbGljYXRpb24iLCAic3VjY2VzcyI6dHJ1ZSwicmVzdWx0Ijo0MiwiaWQiOjAgfQo=" .
```
would log progress injecting the params
```sh
Step 5/7 : ARG params
 ---> Running in bd695783d98c
Removing intermediate container bd695783d98c
 ---> 4557226a40c4
Step 6/7 : ENV params_env=$params
 ---> Running in 400dc16497a9
Removing intermediate container 400dc16497a9
 ---> d4f6dee2cf88
Step 7/7 : ENTRYPOINT ["java", "-Dparams=${params_env}", "-jar", "app.jar" ]
 ---> Running in 46365ccd45f0
Removing intermediate container 46365ccd45f0
 ---> 548da92f039e
Successfully built f9ae39e64c3b
Successfully tagged basic-args-example:latest
```
and run it
```sh
docker run -p 8080:8080 basic-args-example
```

test dockerized
```sh
curl http://localhost:8080/basic
This is my dockerized hparameterized spring application and the result is: 42
```
destroy all started containers and images
```sh
docker container prune -f
docker image prune -f
```
If the `prune` command is not desirable, stop and clean individual container by name
```sh
CONTAINER='basic-args'
ID=$(docker ps | grep $CONTAINER | awk '{print $1}')
docker stop $ID
docker rm $ID
docker image prune -f
```
#### Passing Arguments with run command

Alternatively one can omit the java property definition arguments from `Dockerfile` and replace with a vanilla shell script passing the run arguments to java command:

```sh
java $* -jar app.jar
```
- see `Dockerfile.noargs`. No parameters are needed when building the image
```sh
docker build -f Dockerfile.noargs  -t basic-args-example .
```
and instead define every `-D` options as the `docker run` command argument together with a dummy one:
```sh
echo '{"name":"my parameterized at run time", "success":true,"result":17,"id":1 }' | base64
```
```sh
docker run -it -d -p 8080:8080 --rm basic-args-example _ -Dparams=eyJuYW1lIjoibXkgcGFyYW1ldGVyaXplZCBhdCBydW4gdGltZSIsICJzdWNjZXNzIjp0cnVlLCJyZXN1bHQiOjE3LCJpZCI6MSB9Cg==
```
```sh
curl http://localhost:8080/basic
```
```sh
This is my parameterized at run time and the result is: 17
```

### Passing Arguments, Exercise Repeated

`echoargs.sh`:
```sh
#!/bin/sh
ARGS="$@";
ARGS=${ARGS:-world};
echo "hello ${ARGS}";
```
`Dockerfile`:
```text
FROM alpine:3.9.5
ADD echoargs.sh /tmp
CMD /tmp/echoargs.sh
```
```sh
docker build -t echo4 -f Dockerfile.echo4 .
```
```sh
docker run -it echo4 /tmp/echoargs.sh -cnt 10
```
```text
hello -cnt 10
```

`Dockerfile.echo3`:


```
FROM alpine:3.9.5
ADD echoargs.sh /tmp
ENTRYPOINT /tmp/echoargs.sh $@
```

```sh
docker build -t echo3 -f Dockerfile.echo3 .
```
```sh
docker run -it echo3 "" -cnt 10
```
```text
hello -cnt 10
```

### See Also
   * [tutorial](https://howtodoinjava.com/spring-boot2/application-arguments/) for dealing with the application runtime arguments in a `@Component`
   * discussion of [shell entrypoints](https://stackoverflow.com/questions/37904682/how-do-i-use-docker-environment-variable-in-entrypoint-array) passing in environments
   * vanila coding intterview-grade [discussion](https://www.cyberforum.ru/shell/thread2705261.html) (in Russian)
   * spring boot application [dealing with](https://www.logicbig.com/tutorials/spring-framework/spring-boot/app-args.html) application arguments
   * [stackoverflow](https://stackoverflow.com/questions/34324277/how-to-pass-arg-value-to-entrypoint)
   * [guide to spring @Value]( https://www.baeldung.com/spring-value-annotation) annotations
   * improved code to [preserve double quotes](https://stackoverflow.com/questions/3755772/how-to-preserve-double-quotes-in-in-a-shell-script) of needed elements of $@ in a shell script
   * another [example application](https://github.com/lordoftheflies/example-spring-boot-with-command-line-arguments) for demonstration the usage of command line arguments with Spring Boot

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

