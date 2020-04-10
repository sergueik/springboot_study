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
* list appliocations in manager
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

### Cleanup
```sh
docker stop $NAME
docker container prune -f
docker image rm $IMAGE
```
### See Also:

  * [Guide to Tomcat Manager Application](https://www.baeldung.com/tomcat-manager-app)
  * `HttpServlet`  [life cycle](https://www.mulesoft.com/tcat/tomcat-servlet)

