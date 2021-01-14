### Info

Origin: [Centos7/JDK8 tcserver 3.5](https://github.com/rbosse/pivotal-tc-server)
### Usage
```sh
export IMAGE_NAME=tcserver-orig
docker build -t $IMAGE_NAME -f Dockerfile.orig .
docker run -it -t $IMAGE_NAME
```
The original image has some issues with launching `tc_server`...

* build image
```sh
export IMAGE_NAME='tcserver-try2'
docker build -t $IMAGE_NAME -f Dockerfile .
docker container ls -a | grep  $IMAGE_NAME | awk '{print $1}'  | xargs docker container rm
```
```sh
docker run -it -p 8080:8080  -t $IMAGE_NAME
```
if it does not work, comment the
```sh
CMD /web/tcserver/01/bin/tcruntime-ctl.sh start && tail -f /web/tcserver/01/logs/catalina.out
```
 line and launch the tc_server  interactively:

```sh
docker run -it -p 8080:8080  -t $IMAGE_NAME sh
```
```sh
/web/tcserver/01/bin/tcruntime-ctl.sh start /web/tcserver/01
```
and stay in the shell





### See Also

  * https://tcserver.docs.pivotal.io/4x/docs-tcserver/topics/tcserver-with-docker.html
  * https://github.com/rbosse/pivotal-tc-server/blob/master/Dockerfile
  * https://dzone.com/articles/running-spring-boot-in-a-docker-container
  * https://phoenixnap.com/kb/install-java-on-centos
  * https://stackshare.io/stackups/apache-httpd-vs-pivotal-web-services
  * https://tcserver.docs.pivotal.io/3x/docs-tcserver/topics/intro-getting-started.html
