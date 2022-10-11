### Info

replica ot plain Servlet based REST service backed by PostgreSQL JDBC [project](https://github.com/markuszver/skytecapp)
### Usage
```sh
mvn package
```
```sh
docker build -t test-p -f Dockerfile  .
```
```sh
docker run -p 8080:8080 -d test-p
```

### Issues

when trying to access servlet
```sh
curl http://192.168.0.64:8080/demo/new-transaction
```
get an exception occationally
```text
java.lang.ClassNotFoundException: jakarta.servlet.http.HttpServlet
	org.apache.catalina.loader.WebappClassLoaderBase.loadClass(WebappClassLoaderBase.java:1364)
	org.apache.catalina.loader.WebappClassLoaderBase.loadClass(WebappClassLoaderBase.java:1185)
	java.lang.ClassLoader.defineClass1(Native Method)
	java.lang.ClassLoader.defineClass(ClassLoader.java:763)
	java.security.SecureClassLoader.defineClass(SecureClassLoader.java:142)
	org.apache.catalina.loader.WebappClassLoaderBase.findClassInternal(WebappClassLoaderBase.java:2401)
	org.apache.catalina.loader.WebappClassLoaderBase.findClass(WebappClassLoaderBase.java:859)
	org.apache.catalina.loader.WebappClassLoaderBase.loadClass(WebappClassLoaderBase.java:1333)
	org.apache.catalina.loader.WebappClassLoaderBase.loadClass(WebappClassLoaderBase.java:1185)
	org.apache.catalina.authenticator.AuthenticatorBase.invoke(AuthenticatorBase.java:493)
	org.apache.catalina.valves.ErrorReportValve.invoke(ErrorReportValve.java:81)
	org.apache.catalina.valves.AbstractAccessLogValve.invoke(AbstractAccessLogValve.java:660)
	org.apache.catalina.connector.CoyoteAdapter.service(CoyoteAdapter.java:343)
```
indicating it is not packaged properly
 and  most of the time 404 indicating the servlet is not mapped correctly
```text
The origin server did not find a current representation for the target resource or is not willing to disclose that one exists.
```
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
