### Info

 * basic tomcat servlet example running on Alpine [Tomcat 8.0](https://hub.docker.com/layers/tomcat/library/tomcat/8.0-alpine/images/sha256-65e0ad209ec9ef5ab03fa7bff6f6e1374ce3d7cd7baad00e72162ede7505feb8?context=explore) and [Tomcat 8.5](https://hub.docker.com/layers/tomcat/library/tomcat/8.5-alpine/images/sha256-8b8b1eb786b54145731e1cd36e1de208d10defdbb0b707617c3e7ddb9d6d99c8?context=explore)

### Usage
*  build
```sh
mvn clean package
```
* package into container, very clean
```sh
export IMAGE='basic-servlet'
export NAME='example-servlet'
docker container rm -f $(docker container ls -a | grep $NAME | awk '{print $1}')
docker image prune -f
docker image rm -f $IMAGE
docker build -t $IMAGE -f Dockerfile .
docker run --name $NAME -p 8080:8080 -d $IMAGE
```

* test the servlet

```sh
curl -s http://$(hostname -i):8080/demo/hello-servlet
```sh
- following the `HelloServlet` class annotation:
```java
@WebServlet("/hello-servlet")
public class HelloServlet extends HttpServlet 
```

```
curl -s http://$(hostname -i):8080/demo/hello
```
- following the `web.xml` mapping:
```xml
<servlet>
  <servlet-name>HelloServlet</servlet-name>
  <servlet-class>example.HelloServlet</servlet-class>
</servlet>
<servlet-mapping>
  <servlet-name>HelloServlet</servlet-name>
  <url-pattern>/hello</url-pattern>
</servlet-mapping>
```
both print the plain HTML message
```html
<html><body>
<h1>Hello World!</h1>
</body></html>
```

and logs to console
```sh
docker logs $NAME
```
```text
15-Oct-2021 23:34:47.906 INFO [main] org.apache.catalina.startup.Catalina.start Server startup in 3588 ms
HelloServlet
```
alternarively can open the URL in the browser `http://192.168.0.29:8080/demo/` and click on the __Hello Servlet__ link


### Troubleshooting
* connect to container
```sh
docker exec -it $NAME sh
```
* inspect webapps

### Issues


if switched to Java __1.8__ or later, the Tomcat 8.0 will fail in runtime
```
java.lang.UnsupportedClassVersionError: example/HelloServlet : Unsupported major.minor version 52.0 (unable to load class example.HelloServlet)
```
towork with tomcat 8.0 image use `java7` profile
```sh
mvn -Pjava7 clean package
```
and `Dockerfile.tomcat8.0`:
```
export IMAGE='basic-servlet'
export NAME='example-servlet'
docker container rm -f $(docker container ls -a | grep $NAME | awk '{print $1}')
docker image prune -f
docker image rm -f $IMAGE
docker build -t $IMAGE -f Dockerfile.tomcat8.0 .
docker run --name $NAME -p 8080:8080 -d $IMAGE
```

can also use the `Dockerfile.tomcat8.5` instead of `Dockerfile` for java 1.8 / tomcat 8.5 test

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)




