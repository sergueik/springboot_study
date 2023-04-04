### Info

This directory containes basic static page JSP Tomcat application packaged into war file.
The page compiles and executes basic Java code which prints system environment:
```java
key =  "CATALINA_HOME";
out.println(key + " = " + System.getenv(key));
```
and properties parameters:

```java
String propertiesFile = "application.properties";
String propertyName = "application.setting";
InputStream input = Thread.currentThread().getContextClassLoader().getResourceAsStream(propertiesFile);
Properties properties = new Properties();
properties.load(input);
out.println(propertyName + " = " + properties.getProperty(propertyName));
```
and reads the contents of the local file:
```java
String propertiesPath = getServletContext().getRealPath(propertiesFile);
InputStream input = new FileInputStream(propertiesPath);
Properties properties = new Properties();
properties.load(input);
out.println("Property file \"" + propertiesPath  + "\" " + propertyName + " = " + properties.getProperty(propertyName));
```

### Usage
* test locally
```sh
mvn tomcat:run-war
```
NOTE: currently the JSP page request is leading to Internal Server Error:
```sh
curl -I http://localhost:8080/demo/
```
```sh
HTTP/1.1 500 Internal Server Error
Server: Apache-Coyote/1.1
Content-Type: text/html;charset=utf-8
Content-Length: 2200
Date: Fri, 30 Jul 2021 00:37:14 GMT
Connection: close
``` 
but the explicitly provided static page url works fine:
```sh
curl -I http://localhost:8080/demo/top_example.html
```
```sh
HTTP/1.1 200 OK
```
* package for deployment in container
```sh
IMAGE=basic-jsp
mvn package
```
```sh
docker container ls -a | grep $IMAGE| awk '{print $1}' | xargs docker container rm -f
```
```sh
IMAGE=basic-jsp 
NAME='basic-jsp-container'
docker build -t $IMAGE -f Dockerfile . 
docker container rm -f $NAME
docker run --name $NAME -p 127.0.0.1:8080:8080 -d $IMAGE start
docker logs $NAME
```
* open the index page (note the trailing slash)
```sh
curl http://127.0.0.1:8080/demo/
```
will print

```html
html><body><pre>Server:f774929da86b
Request URL: http://127.0.0.1:8080/demo/
Environment:
APP_SERVER = value
CATALINA_HOME = /opt/tomcat
WINDIR = null
CLASSPATH = :/opt/tomcat/conf:/opt/tomcat/bin/bootstrap.jar:/opt/tomcat/bin/tomcat-juli.jar
application.value = ${env:APP_SERVER}
Property file "/usr/local/tomcat/webapps/demo/application.properties" application.value = ${env:APP_SERVER}
</pre></body></html>
```
- no token expansion observed

 * demo [app](https://github.com/vborrego/jsp-example) with bean / handler integration

### Work in Progress

Attempt to use native Tomcat [Property replacements](https://tomcat.apache.org/tomcat-8.5-doc/config/systemprops.html#Property_replacements) in custom application propetis file does not work.
See Also:
Note: some links still recommend using inner class
https://stackoverflow.com/questions/53921375/tomcat-overriding-catalina-properties-from-commandline
https://stackoverflow.com/questions/11926181/environment-system-variables-in-server-xml
- references [inner class](https://github.com/apache/tomcat/blob/8.5.x/java/org/apache/tomcat/util/digester/Digester.java#L174). This is not needed because nothing is overriden by the inner class
`java/org/apache/tomcat/util/digester/Digester$EnvironmentPropertySource`:
 

# 
```java 
public static class EnvironmentPropertySource extends org.apache.tomcat.util.digester.EnvironmentPropertySource
```
however test are indicating the property expansion to not take place:

With `application.properties`:
```sh
application.setting = ${APP_SERVER}
```
or
```sh
application.setting = ${application.value}
application.value=${env:APP_SERVER}
```
the call
```sh
curl http://localhost:8080/demo/top_example.html

```
shows
```sh
Request URL: http://localhost:8080/demo/
Environment:
APP_SERVER = value
application.value = ${env:APP_SERVER}
application.value(from file) = ${env:APP_SERVER}
```
Dropping the prefix does not help. Note: the regular token replacement works:
```sh
application.setting = ${application.value}
application.value=${env:APP_SERVER} 
```
resolve `application.setting` to `${env:APP_SERVER}` (with or without prefix) verbatim.

The desperate option is to explicitly include the call from into the applicationsince this is what tomcat itself is doing
```java
 @Override
    public String getProperty(String key, ClassLoader classLoader) {
        if (classLoader instanceof PermissionCheck) {
            Permission p = new RuntimePermission("getenv." + key, null);
            if (!((PermissionCheck) classLoader).check(p)) {
                return null;
            }
        }
        return System.getenv(key);
    }
```
*  https://github.com/apache/tomcat/blob/8.5.x/java/org/apache/tomcat/util/digester/EnvironmentPropertySource.java
#

![screenshot 1](https://github.com/sergueik/springboot_study/blob/master/basic-jsp/screenshots/top_parent_message.png)

![screenshot 2](https://github.com/sergueik/springboot_study/blob/master/basic-jsp/screenshots/page_layout.png)

![screenshot 3](https://github.com/sergueik/springboot_study/blob/master/basic-jsp/screenshots/nested_frame_message.png)
### See Also

  * demo [app](https://github.com/vborrego/jsp-example) with bean / handler integration
  * https://stackoverflow.com/questions/5386508/displaying-java-property-in-jsp
  * https://tcserver.docs.pivotal.io/4x/docs-tcserver/topics/encoding-properties.html
  * https://stackoverflow.com/questions/1140653/how-to-load-a-properties-file-into-a-jsp
  * https://javaworld-abhinav.blogspot.com/2019/05/loading-configurationproperties-files.html
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

