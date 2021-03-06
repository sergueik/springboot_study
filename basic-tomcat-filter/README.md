### Info

This directory contins a replica of the [Headers Servlet Filter](https://github.com/ggrandes/headers-servlet-filter) project

It is used with Docker basic tomcat image - the build places the filter jar intotomcat catalina lib direcotry of a stock Docker base image, and updates the `web.xml` in the same then builds container and executes the functionality validation test. The filter class will read configuration and provide the extra HTTP response headers where applicable

For dealing with XML configuration a designated tool is used (classes written in Java). Alternatively one may copy the artifact from the container, modify and plant it back

### Testing

* pick one stock Tomcat 8 Docker image
```sh
docker pull tomcat:8.5.27-jre8-alpine
```
* launch unmodified
```sh
docker run -p 8080:8080 -it --rm tomcat:8.5.27-jre8-alpine /usr/local/tomcat/bin/catalina.sh run
```
* confirm it is running and visible on host forwarded port. The command
```sh
netstat -antp | grep 8080
```
will show
```sh
tcp6       0      0 :::8080   :::*  LISTEN      4159/docker-proxy
```
```sh
curl -I -k http://localhost:8080
```
respods with
```sh
HTTP/1.1 200
```
but tomcat has no context served (no war deployed) on a `dummy` route yet
```sh
curl -I -k http://localhost:8080/dummy/
```
```sh
http://localhost:8080/dummy/
HTTP/1.1 404
Transfer-Encoding: chunked
Date: Wed, 25 Mar 2020 22:32:02 GMT
```
* deploy `dummy` Tomcat HTTP headers producer application into the container:

```sh
pushd webapp
mvn clean package
popd
```

```sh
docker cp webapp/target/dummy.war $(docker ps -q|head -1):/usr/local/tomcat/webapps/
```
* confirm the app deployed and handles the request:
```sh
curl http://localhost:8080/dummy/
```
```xml
<?xml version="1.0"?>
<html>
  <body>
    <h2>Dummy</h2>
  </body>
</html>
```
but observe regular response headers:
```sh
curl -I http://localhost:8080/dummy/
HTTP/1.1 200
Set-Cookie: JSESSIONID=E3470D4339275A2EFE071E98928519BD;path=/dummy/;HttpOnly
Content-Type: text/html;charset=ISO-8859-1
Transfer-Encoding: chunked
```
with a random `JSESSIONID`

### Run Dockerized Filter class

* build app
```sh
mvn clean package
```

* collect vanilla `web.xml` from inside the image:
```sh
CONTAINER=$(docker ps -q)
docker cp $CONTAINER:/usr/local/tomcat/conf/web.xml .
```
* modify the `web.xml` manually (automation of this step is described below)

```sh
xmllint --xpath '//*[local-name()="filter-name" and text() = "responseHeadersFilter"]' web.xml
```
will respond with

```sh
XPath set is empty
```
* Add filter to `web.xml` configuration
```sh
python modify_web_xml.py -i web.xml -o new.xml
```
* confirm the configuration has changed:
`sh
xmllint --xpath '//*[local-name()="filter-name" or local-name()="filter-mapping"][text() = "responseHeadersFilter"]/..' new.xml
```
it will print the injected XML (the console output will have whitespaces and newlines collapsed, here formatted for better readability)
```xml
<filter>
  <filter-name>responseHeadersFilter</filter-name>
  <filter-class>example.ResponseHeadersFilter</filter-class>
  <init-param>
    <param-name>Expires</param-name>
    <param-value>0</param-value>
  </init-param>
</filter>
  <filter-mapping>
  <filter-name>responseHeadersFilter</filter-name>
  <url-pattern>/*</url-pattern>
</filter-mapping>
```
* build image that is capable to update the configuration during Docker build process, executing inside the container

Alternatively one can run Java class from [setuptool](https://github.com/sergueik/springboot_study/tree/master/basic-tomcat-filter/setuptool) on the instance injecting the filter class XML servlet configuration into the tomcat `web.xml` during the Docker build.

* build utility
```sh
pushd setuptool
mvn clean package
popd
```
stop the conaianer (will be auto-removed)
```sh
CONTAINER=$(docker ps -q)
docker stop $CONTAINER
```

```sh
export NAME='example-tomcat-filter'
docker build -t $NAME -f Dockerfile .
docker run -p 8080:8080 -d -t $NAME
```
```sh
export CONTAINER=$(docker container ls | grep $NAME| awk '{print $1}')
```

* Manually deploy a dummy war (may choose to deploy a real application):
```sh
pushd webapp
mvn clean package
popd
```

export NAME='example-tomcat-filter'
docker build -t $NAME -f Dockerfile .
docker run -p 8080:8080 -d -t $NAME
```sh
export CONTAINER=$(docker container ls | grep $NAME| awk '{print $1}')
docker cp webapp/target/dummy.war $CONTAINER:/opt/tomcat/webapps/
docker cp new.xml $CONTAINER:/opt/tomcat/conf/web.xml
```
verify
```sh
curl -I -k http://localhost:8080/dummy/
```
will show extra headers:
```sh
sergueik@sergueik71:~/src/springboot_study/basic-tomcat-filter/webapp$ curl -I -k http://localhost:8080/dummy/
HTTP/1.1 200
Strict-Transport-Security: max-age=15638400
Cache-Control: no-cache, no-store, must-revalidate, max-age=0
Expires: 0
Public-Key-Pins: pin-sha256="base64+primary=="; pin-sha256="b64+backup=="; max-age=604800
Set-Cookie: JSESSIONID=F063ED0B6E963189B2AB5B0D38A28B42;path=/dummy/;HttpOnly
Content-Type: text/html;charset=ISO-8859-1
Transfer-Encoding: chunked
Date: Wed, 25 Mar 2020 22:32:10 GMT
```
if if does not dispay the extra headers, connect to the container
```sh
docker exec -it $CONTAINER sh
```
and run  the configuration setup tool there:
```sh
setuptool_jar="example.setuptool.jar"
java -cp /tmp/${setuptool_jar} example.MergeDocumentFragments -in /opt/tomcat/conf/web.xml -out /tmp/new.xml
```
if it fails, e.g. with

```sh
Loaded: file:///opt/tomcat/conf/web.xml
Testing local file: jar:file:/tmp/example.setuptool.jar!/fragment.xml
Exception in thread "main" java.lang.ClassCastException: com.sun.org.apache.xerces.internal.dom.DeferredCommentImpl cannot be cast to org.w3c.dom.Element
	at example.MergeDocumentFragments.insertNode(MergeDocumentFragments.java:131)
	at example.MergeDocumentFragments.main(MergeDocumentFragments.java:77)
```
test it locally
```
```
if the filter fails to start
```sh
01-Sep-2020 22:50:24.475 SEVERE [localhost-startStop-2] org.apache.catalina.core.StandardContext.startInternal One or more Filters failed to start. Full details will be found in the appropriate container log file
```
connect to container
```sh
docker exec -it $CONTAINER sh
```
and examine
### Building and running Setup Tool

With the help of [setup tool](https://github.com/sergueik/selenium_java/tree/master/xslt-example)
```sh
setuptool/pom.xml
setuptool/src/main/java/example
setuptool/src/main/java/example/CommandLineParser.java
setuptool/src/main/java/example/MergeDocumentFragments.java
setuptool/src/main/resources/fragment.xml
```
one may avoid copying of `web.xml` from / to the image and modify it in place:
```sh
pushd setuptool
mvn clean package
popd
```
```cmd
ARG setuptool_jar="example.setuptool.jar"
ADD "setuptool/target/${setuptool_jar}" /tmp
CMD java -cp /tmp/${setuptool_jar} example.MergeDocumentFragments -in /opt/tomcat/conf/web.xmll -out /tmp/new.xml
CMD cp /tmp/new.xml /opt/tomcat/conf/web.xml
```
Note: for removal of the filter configuration one need a separate command, availale in the original setuptool project directory, not covered in this project.
### Debugging

* (re)build the image and run
```sh
export NAME='example-tomcat-filter'
export CONTAINER=$(docker container ls | grep $NAME| awk '{print $1}')
docker exec -it $CONTAINER sh
```
in the instance
```
pgrep -l java
1 /opt/jdk/bin/java
ps ax | grep org.apache.catalina.startup.Bootstrap
    1 root :05 /opt/jdk/bin/java -Djava.util.logging.config.file=/opt/tomcat/conf/logging.properties -Djava.util.logging.manager=org.apache.juli.ClassLoaderLogManager -Djdk.tls.ephemeralDHKeySize=2048 -classpath /opt/tomcat/bin/bootstrap.jar:/opt/tomcat/bin/tomcat-juli.jar -Dcatalina.base=/opt/tomcat -Dcatalina.home=/opt/tomcat -Djava.io.tmpdir=/opt/tomcat/temp org.apache.catalina.startup.Bootstrap start
```


* cleanup
```
docker stop $CONTAINER; docker container rm -f $CONTAINER;docker container prune -f
docker image rm -f $NAME; docker image prune -f
```


### TODO

```sh
export NAME='example-tomcat-filter'
export CONTAINER=$(docker container ls | grep $NAME| awk '{print $1}')
docker exec -it $CONTAINER sh
```

```sh
ls -1 /opt/tomcat/lib/*jar /opt/tomcat/bin/*jar  |  while read F ; do cp $F /tmp ;done
```

```sh
for F in annotations-api.jar bootstrap.jar catalina-ant.jar catalina-ha.jar catalina-storeconfig.jar catalina-tribes.jar catalina.jar commons-daemon.jar ecj-4.6.3.jar el-api.jar example.headers-filter.jar example.setuptool.jar jasper-el.jar jasper.jar jaspic-api.jar jsp-api.jar servlet-api.jar tomcat-api.jar tomcat-coyote.jar tomcat-dbcp.jar tomcat-i18n-es.jar tomcat-i18n-fr.jar tomcat-i18n-ja.jar tomcat-jdbc.jar tomcat-jni.jar tomcat-juli.jar tomcat-util-scan.jar tomcat-util.jar tomcat-websocket.jar websocket-api.jar ;do docker cp $CONTAINER:/tmp/$F jars ; done
```
```sh
# docker cp $CONTAINER:/tmp/*jar jars
# Error: No such container:path: f0ec8d0d00ad:/tmp/*jar
```
```sh
for F  in $(ls jars ); do jar tvf jars/$F ; done
```
```sh
for F  in $(ls jars ); do jar tvf jars/$F |  grep -i servlet | grep -q Filter && echo $F ;  done
catalina.jar
servlet-api.jar
```
Issue with tomcat __8.5.61__

* build latest:
```sh
IMAGE=tomcat_8.5.61_jre8_alpine39
docker build -t $IMAGE -f Dockerfile.build .
```

then rebuild the container
With __8.5.61__ it is possible to perform environmenr expansion:
```xml
<filter> 
  <filter-name>responseHeadersFilter</filter-name> 
  <filter-class>example.ResponseHeadersFilter</filter-class> 
  <init-param>
   <param-name>Expires</param-name>
   <param-value>${APP_SERVER}</param-value>
  </init-param>
</filter>
```
and the `setenv.sh`
```sh 
docker exec -t $CONTAINER cat /opt/tomcat/bin/setenv.sh
```
```sh
export CLASSPATH=$CLASSPATH:/opt/tomcat/conf:/opt/tomcat/lib/servlet-api.jar:/opt/tomcat/lib/example.headers-filter.jar
export APP_SERVER=value
```

will result in the header:
```sh
curl -k -I  http://localhost:8080/
```
```sh
HTTP/1.1 200
Expires: value
Content-Type: text/html;charset=UTF-8
Transfer-Encoding: chunked
Date: Fri, 29 Jan 2021 20:54:58 GMT
```

Upgrade  from __8.5.27__ to a later tomcat base image:

```sh
docker cp $CONTAINER:/opt/tomcat/lib/tomcat-util-scan.jar .

jar tvf tomcat-util-scan.jar |  grep -i digester |  grep -i proper
org/apache/tomcat/util/digester/Digester$SystemPropertySource.class
```

leading to
```sh
head /opt/tomcat/logs/catalina.2021-01-29.log
```
```sh
29-Jan-2021 03:21:51.447 SEVERE [main] org.apache.tomcat.util.digester.Digester.<clinit> Unable to load property source[org.apache.tomcat.util.digester.EnvironmentPropertySource].
 java.lang.ClassNotFoundException: org.apache.tomcat.util.digester.EnvironmentPropertySource

```
### See  Also

  * `davidcaste/alpine-tomcat` [Dockerfile](https://github.com/davidcaste/docker-alpine-tomcat/blob/master/tomcat8/Dockerfile.jre8)
  * https://www.journaldev.com/1933/java-servlet-filter-example-tutorial
  * http://www.avajava.com/tutorials/lessons/what-is-a-filter-and-how-do-i-use-it.html%3Fpage%3D2?page=1
  * https://www.moreofless.co.uk/static-content-web-pages-images-tomcat-outside-war/
  * Java Servlet Filter [basics](https://www.codejava.net/java-ee/servlet/how-to-create-java-servlet-filter)

  * [dummy catalina war](https://github.com/deepak2717/TomcatDockerWar) project
  * Log4j [tutorial](https://laliluna.com/articles/posts/log4j-tutorial.html) with Tomcat examples
  * filter in [tomcat configured though code](https://www.baeldung.com/tomcat-programmatic-setup)
  * https://stackoverflow.com/questions/60604514/inject-environment-variables-in-tomcat-catalina-properties-kubernetes
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

