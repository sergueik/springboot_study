### Info

This project contains a [basic tomcat web app project](https://github.com/tongueroo/demo-java) modified to process the tomcat manager initialted shutdown event and placed into a basic alpine tomcat 8 Docker container.

### Usage

* test application locally
```sh
mkdir -p src/main/resources
rm App.log App.log*gz

cp log4j2.xml src/main/resources/
mvn clean tomcat:run-war
```
alternatively 
```sh
chmod o+r App.log*gz
```
* open in the browser or console
```sh
TARGET=localhost
```
if run from the same machine or
```sh
TARGET=192.168.0.64
for i in $(seq 1 20); do curl http://$TARGET:8080/demo/Demo ; done
```
Application will respond with
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
the console will show log:
```sh
17:19:14.341 [http-8080-1] DEBUG example.DemoServlet - init()
17:19:14.380 [http-8080-1] DEBUG example.DemoServlet - GET request=org.apache.catalina.connector.RequestFacade@762a5269 {host=localhost:8080, user-agent=curl/7.58.0, accept=*/*}
17:19:14.381 [http-8080-1] INFO  example.DemoServlet - GET request=org.apache.catalina.connector.RequestFacade@762a5269
```
the App.log will show similar messages and file permissions will be (on a Linux host)
```sh
-rw-r----- 1 sergueik sergueik   183 Jul 26 01:30 App.1.log.gz
-rw-r----- 1 sergueik sergueik   176 Jul 26 01:30 App.2.log.gz
-rw-r----- 1 sergueik sergueik   176 Jul 26 01:30 App.3.log.gz
-rw-r----- 1 sergueik sergueik   337 Jul 26 01:30 App.log
```
* interrupt the application through
`^C`
if will produce the log message from the `destroy()` method before closing
```sh
17:19:23.009 [Thread-2] DEBUG example.DemoServlet - destroy()
```

Move the `log4j2.xml` away from resources directory (the one in the project root is checked in)
and repackage and relaunch explicitly passing the configuration information:
on Windows host
```cmd
del /q src\main\resources\log4j2.xml
mvn -Dlog4j.configurationFile=%CD%\log4j2.xml clean tomcat:run-war
```
on Linux host
```sh
rm -f src/main/resources/log4j2.xml
mvn -Dlog4j.configurationFile=$(pwd)/log4j2.xml clean tomcat:run-war
```
confirm the logging continues to work
### Testing with Docker Container

* build application
```sh
rm -f src/main/resources/log4j2.xml
```
NOTE: we do now want the `log4j2.xml` to be installed by tomcat into application's `WEB-INF/classes` - the purpose of this project is to explore overriding the default locations
```sh
mvn clean package
```
verify the log4j configuration was not included:
```sh
jar tvf target/demo.war | grep log4j2.xml
```
the above command should print nothing. if log4j2.xml inside the jar, repackage clean
* build and deploy Docker image

```sh
export IMAGE='basic-tomcat'
export NAME='example-tomcat'
docker build -t $IMAGE -f Dockerfile .
docker container rm -f $(docker container ls -a | grep $NAME | awk '{print $1}')
docker run --name $NAME -p 8080:8080 -d $IMAGE
```
* verify there is just one instance of `log4j2.xml` configuration file in tomcat in the container:

```sh
export CONTAINER_ID=$(docker container ls -a| grep $NAME| awk '{print $1}')
docker exec -it $CONTAINER_ID sh
#
```
```sh
find /opt/tomcat/ -iname 'log4j2.xml'
/opt/tomcat/conf/log4j2.xml
```
* connect to web application
```sh
curl http://127.0.0.1:8080/demo/Demo?dummy 2>/dev/null| lynx -stdin -dump
```
will print
```sh
You requested=[http://127.0.0.1:8080/demo/Demo?dummy]
message
 message
     __________________________________________________________________

   host       127.0.0.1:8080
   accept     */*
   user-agent curl/7.58.0
```
if open the url in the browser, additional headers will be logged:
```sh
You requested=[http://127.0.0.1:8080/demo/Demo?dummy]
message
sec-fetch-user	?1
connection	keep-alive
accept-language	en-US,en;q=0.9
host	127.0.0.1:8080
sec-fetch-mode	navigate
accept	text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3
user-agent	Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/77.0.3865.121 Safari/537.36 Vivaldi/2.8.1664.44
accept-encoding	gzip, deflate, br
sec-fetch-site	none
upgrade-insecure-requests	1
```
* reconnect into container shell and  examine permissions of the log:
```sh
docker exec -it $CONTAINER_ID sh
```
then confirm tomcat uses the configuration file we confirmed the location earlier (the `/opt/tomcat/conf/log4j2.xml`)
```sh
ps axww | grep tomcat
```
```
1 root      0:13 /usr/lib/jvm/java-1.7-openjdk/jre/bin/java -Djava.util.logging.config.file=/opt/tomcat/conf/logging.properties -Djava.util.logging.manager=org.apache.juli.ClassLoaderLogManager -Dlog4j2.debug=true -Dapp.env=staging -Dlog4j.configurationFile=/opt/tomcat/conf/log4j2.xml -Djdk.tls.ephemeralDHKeySize=2048 -Djava.protocol.handler.pkgs=org.apache.catalina.webresources -Dignore.endorsed.dirs= -classpath /opt/tomcat/bin/bootstrap.jar:/opt/tomcat/bin/tomcat-juli.jar -Dcatalina.base=/opt/tomcat -Dcatalina.home=/opt/tomcat -Djava.io.tmpdir=/opt/tomcat/temp org.apache.catalina.startup.Bootstrap start
```
and the log file is present with the intendet permissions
```
find / -iname 'App.log' -exec stat  -c "%a %U %G %n" {} \;
```
this will display
```sh
640 root root /usr/local/tomcat/App.log
```
similarly 
```sh
find / -iname 'App.*.log*' -exec stat  -c "%a %U %G %n" {} ;
```
will report
```sh
640 root root /usr/local/tomcat/App.3.log.gz
640 root root /usr/local/tomcat/App.2.log.gz
640 root root /usr/local/tomcat/App.1.log.gz
```

NOTE: verified explicitly  that without the log4j2.xml `filePermssions` attribute the `UMASK=0027` fully governs the log permissions:
with `opt/tomcat/bin/setenv.sh`
```sh
export JAVA_OPTS="-Dlog4j2.debug=true -Dapp.env=staging -Dlog4j.configurationFile=$CATALINA_BASE/conf/log4j2.xml"
export UMASK=0027
```
one gets

```sh
-rw-r-----    1 root     root           179 Jul 31 18:34 /usr/local/tomcat/App.1.log.gz
-rw-r-----    1 root     root           177 Jul 31 18:34 /usr/local/tomcat/App.2.log.gz
-rw-r-----    1 root     root           184 Jul 31 18:34 /usr/local/tomcat/App.3.log.gz
-rw-r-----    1 root     root           582 Jul 31 18:34 /usr/local/tomcat/App.log
```

with `opt/tomcat/bin/setenv.sh`
```sh
export JAVA_OPTS="-Dlog4j2.debug=true -Dapp.env=staging -Dlog4j.configurationFile=$CATALINA_BASE/conf/log4j2.xml"
export UMASK=0022
```
one gets

```sh
-rw-r--r--    1 root     root           182 Jul 31 18:47 /usr/local/tomcat/App.1.log.gz
-rw-r--r--    1 root     root           177 Jul 31 18:47 /usr/local/tomcat/App.2.log.gz
-rw-r--r--    1 root     root           184 Jul 31 18:47 /usr/local/tomcat/App.3.log.gz
-rw-r--r--    1 root     root           582 Jul 31 18:47 /usr/local/tomcat/App.log
```
while with
`opt/tomcat/bin/setenv.sh`
```sh
export JAVA_OPTS="-Dlog4j2.debug=true -Dapp.env=staging -Dlog4j.configurationFile=$CATALINA_BASE/conf/log4j2.xml"
export UMASK=0077
```
one gets

```sh
-rw-------    1 root     root           184 Jul 31 18:54 /usr/local/tomcat/App.1.log.gz
-rw-------    1 root     root           177 Jul 31 18:54 /usr/local/tomcat/App.2.log.gz
-rw-------    1 root     root           177 Jul 31 18:54 /usr/local/tomcat/App.3.log.gz
-rw-------    1 root     root           582 Jul 31 18:54 /usr/local/tomcat/App.log
```

* list applications in manager
```sh
docker logs $CONTAINER_ID
```
this will show similar logs alreday seen during maven target run
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
### All commands combined

* rebuild and repackage container with the war
```sh
docker stop $CONTAINER
docker container prune -f
mvn clean package
docker image rm $IMAGE
docker build -t $IMAGE -f Dockerfile .
docker run --name $NAME -p 8080:8080 -d $IMAGE
```
inspect console log:
```sh
docker container prune -f
export CONTAINER=$(docker container ls -a| grep $NAME| awk '{print $1}')
docker logs $CONTAINER
```
will observe
```sh
15:26:48.335 [http-apr-8080-exec-1] DEBUG example.DemoServlet - init()
```
* repeat curl command to invoke the servlet
```sh
curl http://localhost:8080/demo/Demo?foo=bar
```
will see
```sh
15:31:59.319 [http-apr-8080-exec-5] INFO  example.DemoServlet - GET request=org.apache.catalina.connector.RequestFacade@3605e6af
15:32:17.721 [http-apr-8080-exec-7] DEBUG example.DemoServlet - GET request=org.apache.catalina.connector.RequestFacade@3605e6af {host=127.0.0.1:8080, accept=*/*, user-agent=curl/7.58.0}
```
* stop the demo application
```sh
curl -u admin:password http://localhost:8080/manager/text/stop?path=/demo
```
console log will show:
```sh
15:37:01.959 [http-apr-8080-exec-2] DEBUG example.DemoServlet - destroy()
```
* inspect log files
```
docker exec -it $CONTAINER sh
```
in the container shell
```
vi /opt/tomcat/logs/catalina.`date +"%Y-%m-%d"`.log
```
remove the `log4j2.xml` from application container
```sh
docker exec -it $CONTAINER sh
#
rm -f /opt/tomcat/webapps/demo/WEB-INF/classes/log4j2.xml
```
then in the host
```sh
curl -u admin:password http://localhost:8080/manager/text/stop?path=/demo
curl -u admin:password http://localhost:8080/manager/text/start?path=/demo
```
and repeat the servlet invocation:
```sh
curl http://localhost:8080/demo/Demo?foo=bar
```
observe the logging to stop.

copy the  `log4j2.xml` into container lib:

```sh
docker cp src/main/resources/log4j2.xml $CONTAINER:/opt/tomcat/lib
```
observe the logging to resume

### Application Properties

```sh
docker container exec -it  $(docker container ls -a | grep $NAME | awk '{print $1}')  sh
```
in the instance
```
find /opt/tomcat/webapps/demo -iname 'application.properties'
```
```sh
/opt/tomcat/webapps/demo/WEB-INF/classes/application.properties
```

remove
```sh
rm /opt/tomcat/webapps/demo/WEB-INF/classes/application.properties
```
observe
```sh
curl -k  http://localhost:8080/demo/Demo?a=1
```
still be able to load properies:
```html
<hr/>properties: application.setting = value

```
NOTE: token expansion not taking place
after placing
```sh
application.value = value
application.setting = ${application.value}
```
see the raw value w/o token expansion
```html
<hr/>properties: application.setting = ${application.value}
```

### TODO:

Still not properly configured logging. With `log4j2.debug` set to `true` logs numerous attempts to find configuration files

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
the `/opt/tomcat/conf/logging.properties` coming with the base container needs to be removed and not referenced in the launcher command:
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

```sh
docker exec -it $CONTAINER sh
env | grep log4j.configurationFile=
log4j.configurationFile=/conf/log4j2.xm
```

errors observed in
```sh
mvn tomcat:run-war
```
```sh
INFO: validateJarFile(/home/sergueik/src/springboot_study/basic-tomcat-manager/target/example.managed_app/WEB-INF/lib/javax.servlet-api-3.1.0.jar) - jar not loaded. See Servlet Spec 2.3, section 9.7.2. Offending class: javax/servlet/Servlet.class
```
possibly contributing to
```sh
curl  -I -k  http://localhost:8080/demo/Demo?a=123
HTTP/1.1 400 Bad Request
Server: Apache-Coyote/1.1
Transfer-Encoding: chunked
Date: Fri, 15 Jan 2021 20:52:28 GMT
Connection: close
```

### Cleanup
```sh
docker container stop $CONTAINER_ID
docker container rm -f $CONTAINER_ID
docker image prune -f
```

### Runtime Errors
```text
INFO: validateJarFile(/home/sergueik/src/springboot_study/basic-tomcat-manager/target/example.managed_app/WEB-INF/lib/javax.servlet-api-3.1.0.jar) - jar not loaded. See Servlet Spec 2.3, section 9.7.2. Offending class: javax/servlet/Servlet.class
```
thr upgrade to the latest available does not resolve the issue
```text
INFO: validateJarFile(/home/sergueik/src/springboot_study/basic-tomcat-manager/target/example.managed_app/WEB-INF/lib/javax.servlet-api-4.0.1.jar) - jar not loaded. See Servlet Spec 2.3, section 9.7.2. Offending class: javax/servlet/Servlet.class
```
### TODO:

### See Also:
  * official [HOWTO](https://tomcat.apache.org/tomcat-8.5-doc/manager-howto.html#Stop_an_Existing_Application)
  * [Guide to Tomcat Manager Application](https://www.baeldung.com/tomcat-manager-app)
  * `HttpServlet`  [life cycle](https://www.mulesoft.com/tcat/tomcat-servlet)
  * [packaging springboot app in war](https://mkyong.com/spring-boot/spring-boot-deploy-war-file-to-tomcat/)
  * [fixing](https://crunchify.com/java-how-to-configure-log4j-logger-property-correctly/) `ERROR StatusLogger No log4j2 configuration file found. Using default configuration: logging only errors to the console`
  * example project of watching the [ tomcat logging configuration](https://github.com/phoet/tomcat-logging) file
  * log4j tutorial with [Tomcat](https://laliluna.com/articles/posts/log4j-tutorial.html)
  * log4j config [migration](https://logging.apache.org/log4j/2.x/manual/migration.html)
  * the `org.apache.log4j.RollingFileAppender` [tutorial](https://www.baeldung.com/java-logging-rolling-file-appenders)
  * class [javadoc](https://logging.apache.org/log4j/2.x/manual/appenders.html) -  make sure to browse the correct version. The `filePermissions` property only appears in __2.9__
  * managing [log file permissions](https://stackoverflow.com/questions/7893511/permissions-on-log-files-created-by-log4j-rollingfileappender) - besides via `umask` e.f. of `0137` for `640`
  * [umask](https://stackoverflow.com/questions/41975808/set-umask-for-tomcat8-via-tomcat-service) setting in systemd unit and `catalina.sh` 
  * https://howtodoinjava.com/log4j2/log4j2-rollingfileappender-example/
 * [basic servlet intro](https://javarush.ru/groups/posts/523-vashe-pervoe-prilozhenie-s-ispoljhzovaniem-java-servletov) (in Russian)  

### License
This project is licensed under the terms of the MIT license.

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

