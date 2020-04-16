### Info

This project contains a [basic tomcat web app project](https://github.com/tongueroo/demo-java) modified to process the tomcat manager initialted shutdown event and placed into a basic alpine tomcat 8 Docker container.

### Usage

* test application locally
```sh
mvn clean tomcat:run-war
```
open in the browser or console
```sh
curl http://localhost:8080/demo/Demo
```
will show
```html
<body>
You requested=[http://127.0.0.1:8080/demo/Demo?null]
<hr>message
<hr>
<table>
  <tbody>
    <tr>
      <td>accept-language</td>
      <td>en-US,en;q=0.9</td>
    </tr>
    <tr>
      <td>host</td>
      <td>127.0.0.1:8080</td>
    </tr>
    <tr>
      <td>upgrade-insecure-requests</td>
      <td>1</td>
    </tr>
    <tr>
      <td>connection</td>
      <td>keep-alive</td>
    </tr>
    <tr>
      <td>accept-encoding</td>
      <td>gzip, deflate, br</td>
    </tr>
    <tr>
      <td>user-agent</td>
      <td>Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/73.0.3683.105 Safari/537.36 Vivaldi/2.4.1488.40</td>
    </tr>
    <tr>
      <td>accept</td>
      <td>text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8</td>
    </tr>
  </tbody>
</table>
```
### Testing with Docker Container

* build application
```sh
mvn package
```
* build and deploy Docker image

```sh
export IMAGE='basic-tomcat'
export NAME='example-tomcat'
docker build -t $IMAGE -f Dockerfile .
docker run --name $NAME -p 8080:8080 -d $IMAGE
```
* connect to web application
```sh
curl http://127.0.0.1:8080/demo/Demo | lynx -stdin -dump
```
will print
```
You requested=[http://127.0.0.1:8080/demo/Demo?null]
message
accept-language	en-US,en;q=0.9
host	127.0.0.1:8080
upgrade-insecure-requests	1
connection	keep-alive
accept-encoding	gzip, deflate, br
user-agent	Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/73.0.3683.105 Safari/537.36 Vivaldi/2.4.1488.40
accept	text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8
```
* list applications in manager
```sh
export CONTAINER=$(docker container ls -a| grep $NAME| awk '{print $1}')
```
```sh
docker logs $CONTAINER
```
```sh
curl -u admin:password http://localhost:8080/manager/text/list
```
will respond with
```sh
OK - Listed applications for virtual host localhost
/:running:0:ROOT
/manager:running:0:manager
/demo:running:0:demo
/docs:running:0:docs
/examples:running:0:examples
/host-manager:running:0:host-manager
```
* stop the demo application
```sh
curl -u admin:password http://localhost:8080/manager/text/stop?path=/demo
```
will respond with
```
OK - Stopped application at context path /demo
```
* start the demo application
```sh
curl -u admin:password http://localhost:8080/manager/text/start?path=/demo
```
Check the manager logs
```sh
docker exec -it $NAME tail /opt/tomcat/logs/manager.`date +%Y-%m-%d`.log
```
will show
```sh
10-Apr-2020 22:26:50.726 INFO [http-apr-8080-exec-3] org.apache.catalina.core.ApplicationContext.log Manager: stop: Stopping web application '/demo'
10-Apr-2020 22:32:46.485 INFO [http-apr-8080-exec-13] org.apache.catalina.core.ApplicationContext.log Manager: start: Starting web application '/demo'
```

Note: despite adding the method which is supposed to be called during the stopping through manager
```java
public void destroy() {
  System.err.println("Destroyed");
}
```
not seeing any message in console, nor in `catalina.out`
### Use Springboot application war packaging
failing to start in Tomcat:
```sh
FAIL - Application at context path /demo could not be started
FAIL - Encountered exception org.apache.catalina.LifecycleException: Failed to start component [StandardEngine[Catalina].StandardHost[localhost].StandardContext[/demo]]
```
### All commands collapsed

* rebuild and repackage container with the war
```sh
docker stop $CONTAINER
docker container prune -f
mvn clean package
docker image rm $IMAGE
docker build -t $IMAGE -f Dockerfile .
docker run --name $NAME -p 8080:8080 -d $IMAGE
```
* invoke the servlet
```sh
curl http://localhost:8080/demo/Demo?foo=bar
```
inspect console log (temporarily using `error` logging):
```sh
export CONTAINER=$(docker container ls -a| grep $NAME| awk '{print $1}')
docker logs $CONTAINER
```
will show
```sh
ERROR example.DemoServlet - GET request=org.apache.catalina.connector.RequestFacade@29cd287d 
{
accept-language=en-US,en;q=0.9, 
host=127.0.0.1:8080, 
upgrade-insecure-requests=1, 
connection=keep-alive, 
accept-encoding=gzip, deflate, br, 
user-agent=Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/73.0.3683.105 Safari/537.36 Vivaldi/2.4.1488.40, 
accept=text/html, application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;
q=0.8
}
```
* stop the demo application
```sh
curl -u admin:password http://localhost:8080/manager/text/stop?path=/demo
```
console log will show:
```sh
21:33:33.404 [http-apr-8080-exec-3] ERROR example.DemoServlet - destroy()
```
* inspect log files
```
docker exec -it $CONTAINER sh
```
in the container shell
```
vi /opt/tomcat/logs/catalina.`date +"%Y-%m-%d"`.log
```
### Cleanup
```sh
docker stop $NAME
docker container prune -f
docker image rm $IMAGE
```
### TODO:

Still not properly configured logging. With `log4j2.debug` set to `true` logs numwerous attempts to find configuration files

```sh
TRACE StatusLogger Trying to find [log4j2.xml] using context class loader WebappClassLoader
  context: demo
  delegate: false
----------> Parent Classloader:
java.net.URLClassLoader@32cd666f
.
TRACE StatusLogger Trying to find [log4j2.xml] using WebappClassLoader
  context: demo
  delegate: false
----------> Parent Classloader:
java.net.URLClassLoader@32cd666f
 class loader.
TRACE StatusLogger Trying to find [log4j2.xml] using WebappClassLoader
  context: demo
  delegate: false
----------> Parent Classloader:
java.net.URLClassLoader@32cd666f
 class loader.
TRACE StatusLogger Trying to find [log4j2.xml] using ClassLoader.getSystemResource().
```
culminated with
```sh
ERROR StatusLogger No Log4j 2 configuration file found.
Using default configuration (logging only errors to the console),
or user programmatically provided configurations.
Set system property 'log4j2.debug' to show Log4j 2
internal iinitialization loggi
See https://logging.apache.org/log4j/2.x/manual/configuration.html
for instructions on how to configure Log4j 2
```
the `/opt/tomcat/conf/logging.properties` coming with the base container needs to be removed and not referenced by the launcher:
```sh
 /usr/lib/jvm/java-1.7-openjdk/jre/bin/java
 -Djava.util.logging.config.file=/opt/tomcat/conf/logging.properties
 -Djava.util.logging.manager=org.apache.juli.ClassLoaderLogManager
 -Dlog4j2.debug=true
 -Dapp.env=staging
 -Dlog4j.configuration=/opt/tomcat/conf/log4j2.xml
 -Djdk.tls.ephemeralDHKeySize=2048
 -Djava.protocol.handler.pkgs=org.apache.catalina.webresources
 -Dignore.endorsed.dirs=
 -classpath /opt/tomcat/bin/bootstrap.jar:/opt/tomcat/bin/tomcat-juli.jar
 -Dcatalina.base=/opt/tomcat
 -Dcatalina.home=/opt/tomcat
 -Djava.io.tmpdir=/opt/tomcat/temp
 org.apache.catalina.startup.Bootstrap start
```
```sh
docker run -e LOGGING_CONFIG="-Djava.util.logging.config.file=\$CATALINA_BASE/conf/log4j2.xml" --name $NAME -p 8080:8080 -d $IMAGE
```
### See Also:

  * [Guide to Tomcat Manager Application](https://www.baeldung.com/tomcat-manager-app)
  * `HttpServlet`  [life cycle](https://www.mulesoft.com/tcat/tomcat-servlet)
  * [packaging springboot app in war](https://mkyong.com/spring-boot/spring-boot-deploy-war-file-to-tomcat/)
  * [fixing](https://crunchify.com/java-how-to-configure-log4j-logger-property-correctly/) `ERROR StatusLogger No log4j2 configuration file found. Using default configuration: logging only errors to the console`
  * example project of watching the [ tomcat logging configuration](https://github.com/phoet/tomcat-logging) file
  * log4j tutorial with [Tomcat](https://laliluna.com/articles/posts/log4j-tutorial.html)

