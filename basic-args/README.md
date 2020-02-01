### Info

Springboot Docker basic argument exercise project cloned from
[lordoftheflies/example-spring-boot-with-command-line-arguments](https://github.com/lordoftheflies/example-spring-boot-with-command-line-arguments)
Modified to pass an arbirtary data structure wrapped in json and subsequntly base 64 encoded, through  SpringBoot - hopefully
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
destroy all started containers and images
```sh
docker contained prune -f
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

### See Also
   * [tutorial](https://howtodoinjava.com/spring-boot2/application-arguments/) for dealing with the application runtime arguments in a `@Component`
   * discussion of [shell entrypoints](https://stackoverflow.com/questions/37904682/how-do-i-use-docker-environment-variable-in-entrypoint-array) passing in environments
   * spring boot application [dealing with](https://www.logicbig.com/tutorials/spring-framework/spring-boot/app-args.html) application arguments

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
