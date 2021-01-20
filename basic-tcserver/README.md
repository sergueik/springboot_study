### Info

Origin: [Centos7/JDK8 tcserver 3.5](https://github.com/rbosse/pivotal-tc-server)
### Usage
```sh
export IMAGE_NAME=tcserver-orig
export NAME='tcserver-container'
docker build -t $IMAGE_NAME -f Dockerfile.orig .
docker run -d --name $NAME -t $IMAGE_NAME
docker logs $NAME
```
The original image has some issues with launching `tc_server`...

* build image
```sh
export IMAGE_NAME='tcserver-try2'
docker build -t $IMAGE_NAME -f Dockerfile.centos7 .
docker container ls -a | grep  $IMAGE_NAME | awk '{print $1}'  | xargs docker container rm
```
```sh
docker run -d --name $NAME -t $IMAGE_NAME
docker logs $NAME
```
if it does not work, comment the
```sh
CMD /web/tcserver/01/bin/tcruntime-ctl.sh start && tail -f /web/tcserver/01/logs/catalina.out
```
 line and launch the tc_server  interactively:

```sh
export NAME='tcserver-container'
docker exec -it $NAME sh
```
```sh
/web/tcserver/01/bin/tcruntime-ctl.sh start /web/tcserver/01
```
and stay in the shell

```sh
docker cp dummy.war $NAME:/web/tcserver/01/webapps
```
and restart the server
```sh
export NAME='tcserver-container'
docker exec -it $NAME sh
```
```sh
/web/tcserver/01/bin/tcruntime-ctl.sh stop
/web/tcserver/01/bin/tcruntime-ctl.sh start
```
### Alpine 
Note: Tanzu Network membership is required to download 'VMware tc Server Developer Edition'
build vanilla alpine JRE image and place the tc server archive inside
```sh
export IMAGE_NAME='tcserver-alpine'
docker image rm  $IMAGE_NAME
docker build -t $IMAGE_NAME -f Dockerfile .
docker image inspect $IMAGE_NAME
export NAME='tcserver-alpine-container'
docker container rm $NAME
```
```sh
docker run --name $NAME -it -t $IMAGE_NAME /bin/sh
```
follow the guide
```sh
export APP_DIR='/opt/pivotal/tcserver'
export ROOT_DIR="$APP_DIR/instances"
expoer SITE='demo-instance'
mkdir $APP_DIR
chdir $APP_DIR
mv /tcserver.tar.gz/pivotal_tcserver/* .
export PATH=$PATH:$(pwd)/developer-4.1.5.RELEASE
which tcserver
tcserver create $SITE
mkdir $ROOT_DIR/$SITE/webapps/dummy
```
copy the jsp from the host:
```sh
docker cp ../basic-jsp/src/main/webapp/index.jsp  $NAME:/opt/pivotal/tcserver/instances/demo-instance/webapps/dummy
docker cp ../basic-jsp/src/main/webapp/application.properties  $NAME:/opt/pivotal/tcserver/instances/demo-instance/conf
```
in the container
```sh
sed -i "s|/opt/tomcat|$ROOT_DIR/$SITE|g' $ROOT_DIR/$SITE/webapps/dummy/index.jsp
echo "CLASSPATH=$ROOT_DIR/$SITE/conf" | tee -a $ROOT_DIR/$SITE/bin/setenv.sh
```
```sh
tcserver start $SITE
```
ignore the failure message
at this time the `application.properties` contains
```sh
application.value = some data
application.setting = ${application.value}
```
verify token expansion
```sh
curl http://localhost:8080/dummy/
```
```sh
<html><body><pre>Server:35975f9e99a9
Request URL: http://localhost:8080/dummy/
Environment:
APP_SERVER = null
application.value = some data
application.value(from file) = some data
</pre></body></html>
```
### See Also

  * https://tcserver.docs.pivotal.io/4x/docs-tcserver/topics/tcserver-with-docker.html
  * https://github.com/rbosse/pivotal-tc-server/blob/master/Dockerfile
  * https://dzone.com/articles/running-spring-boot-in-a-docker-container
  * https://phoenixnap.com/kb/install-java-on-centos
  * https://stackshare.io/stackups/apache-httpd-vs-pivotal-web-services
  * https://tcserver.docs.pivotal.io/3x/docs-tcserver/topics/intro-getting-started.html
