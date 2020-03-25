### Info

This directory contins a replica of the project  [](https://github.com/ggrandes/headers-servlet-filter)

It is used with Docker basic tomcat image - the build places the filter jar into catalina lib and updates the `web.xml` then runs the test



### Testing

```sh
docker pull davidcaste/alpine-tomcat
```
```sh
docker run -p 8080:8080 -it --rm davidcaste/alpine-tomcat /opt/tomcat/bin/catalina.sh run
```
then it will be running
```
netstat -antp | grep 8080
```
will show
```
tcp6       0      0 :::8080   :::*  LISTEN      4159/docker-proxy

```
but it has no context (no war deployed)
```sh
curl -I -k http://localhost:8080/dummy/
```
```sh
http://localhost:8080/dummy/
HTTP/1.1 404
Transfer-Encoding: chunked
Date: Wed, 25 Mar 2020 22:32:02 GMT
```
### Run Dockerized Filter class

* build app
```sh
mvn clean package
```

* build image
```sh
export NAME='example-tomcat-filter'
export CONTAINER=$(docker container ls | grep $NAME| awk '{print $1}')
```

```sh
docker build -t $NAME -f Dockerfile .
docker run -p 8080:8080 -d -t $NAME
```
* collect vanilla `web.xml` from inside the image:
```
docker cp $CONTAINER:/opt/tomcat/conf/web.xml .
```
* modify the `web.xml` (manually  - automation is work in prorgess)

```sh
xmllint --xpath '//*[local-name()="filter-name"  and text() = "ResponseHeadersFiltezr"]' web.xml
```
will respond

```
XPath set is empty
```
```
xmllint --xpath '//*[local-name()="filter-name" or local-name()="filter-mapping"][text() = "ResponseHeadersFilter"]/..' web.xml
```


```xml
<filter>
    <filter-name>ResponseHeadersFilter</filter-name>
    <filter-class>org.javastack.servlet.filters.ResponseHeadersFilter</filter-class>
    <!-- headers are set, replacing any previous header with his name -->
    <!-- Cache Control / Expiration -->
    <init-param>
        <param-name>Expires</param-name>
        <param-value>0</param-value>
    </init-param>
    <init-param>
        <param-name>Cache-Control</param-name>
        <param-value>no-cache, no-store, must-revalidate, max-age=0</param-value>
    </init-param>
    <!-- SSL/TLS Security -->
    <init-param>
        <param-name>Strict-Transport-Security</param-name>
        <param-value>max-age=15638400</param-value>
    </init-param>
    <init-param>
        <param-name>Public-Key-Pins</param-name>
        <param-value>pin-sha256="base64+primary=="; pin-sha256="b64+backup=="; max-age=604800</param-value>
    </init-param>
</filter><filter-mapping>
    <filter-name>ResponseHeadersFilter</filter-name>
    <url-pattern>/*</url-pattern>
</filter-mapping>
```
* (re)build the image and run

```sh
docker exec -it $CONTAINER sh
```
in the instance
```
pgrep -l java
1 /opt/jdk/bin/java
ps ax | grep org.apache.catalina.startup.Bootstrap
    1 root :05 /opt/jdk/bin/java -Djava.util.logging.config.file=/opt/tomcat/conf/logging.properties -Djava.util.logging.manager=org.apache.juli.ClassLoaderLogManager -Djdk.tls.ephemeralDHKeySize=2048 -classpath /opt/tomcat/bin/bootstrap.jar:/opt/tomcat/bin/tomcat-juli.jar -Dcatalina.base=/opt/tomcat -Dcatalina.home=/opt/tomcat -Djava.io.tmpdir=/opt/tomcat/temp org.apache.catalina.startup.Bootstrap start
```

Manual add a dummy war:
```show
pushd webapps
mvn clean package
```

```sh
docker cp target/dummy.war $CONTAINER:/opt/tomcat/webapps/
```

```
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

* cleanup
```
docker stop $CONTAINER; docker container rm -f $CONTAINER;docker container prune -f
docker image rm -f $NAME; docker image prune -f
```
### See  Also

  * `davidcaste/alpine-tomcat` [Dockerfile]https://github.com/davidcaste/docker-alpine-tomcat/blob/master/tomcat8/Dockerfile.jre8
  * https://www.journaldev.com/1933/java-servlet-filter-example-tutorial
  * http://www.avajava.com/tutorials/lessons/what-is-a-filter-and-how-do-i-use-it.html%3Fpage%3D2?page=1
  * https://www.moreofless.co.uk/static-content-web-pages-images-tomcat-outside-war/


### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)