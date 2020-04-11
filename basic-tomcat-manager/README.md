### Info

This project contains a [basic tomcat web app project](https://github.com/tongueroo/demo-java) modified to process the tomcat manager initialted shutdown event and placed into a basic alpine tomcat 8 Docker container.

### Usage
*  build application
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
connect to just deploued web application
```sh
curl http://127.0.0.1:8080/demo
```
will print
```sh
<html>
<body>
<div>index page</div>
</body>
</html>
```
* list applications in manager
```sh
export CONTAINER=$(docker container ls -a| grep $NAME| awk '{print $1}')
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
21:33:04.412 [http-apr-8080-exec-1] ERROR example.DemoServlet - responding to GET request=org.apache.catalina.connector.RequestFacade@2ee23f1d
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
### See Also:

  * [Guide to Tomcat Manager Application](https://www.baeldung.com/tomcat-manager-app)
  * `HttpServlet`  [life cycle](https://www.mulesoft.com/tcat/tomcat-servlet)
  * [packaging springboot app in war](https://mkyong.com/spring-boot/spring-boot-deploy-war-file-to-tomcat/)
  * [fixing](https://crunchify.com/java-how-to-configure-log4j-logger-property-correctly/) `ERROR StatusLogger No log4j2 configuration file found. Using default configuration: logging only errors to the console`
  * example project of watching the [ tomcat logging configuration](https://github.com/phoet/tomcat-logging) file
