### Info

This directory containes basic JSP Tomcat application packaged into war file.
### Usage

```sh
IMAGE=basic-jsp
mvn package
```
```sh
docker container ls -a | grep $IMAGE| awk '{print $1}' | xargs docker container rm -f
```
```sh
IMAGE=basic-jsp; NAME='basic-jsp-container'; docker build -t $IMAGE -f Dockerfile . ; docker container rm -f $NAME ; docker run --name $NAME -p 127.0.0.1:8080:8080 -d $IMAGE start
```
```sh
curl http://127.0.0.1:8080/demo
```
will reply with

```html
<html><body><pre>Server:a8de1a512b97
Request URL: http://localhost:8080/demo/
APP_SERVER = null
application.setting = ${application.value}
application.setting(from file) = ${application.value}
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
curl http://localhost:8080/demo/
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

### See Also

  * demo [app](https://github.com/vborrego/jsp-example) with bean / handler integration
  * https://stackoverflow.com/questions/5386508/displaying-java-property-in-jsp
  * https://tcserver.docs.pivotal.io/4x/docs-tcserver/topics/encoding-properties.html
  * https://stackoverflow.com/questions/1140653/how-to-load-a-properties-file-into-a-jsp
  * https://javaworld-abhinav.blogspot.com/2019/05/loading-configurationproperties-files.html
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

