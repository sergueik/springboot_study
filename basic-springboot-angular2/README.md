### Info

Replica of [Angular2-springboot](https://github.com/arifcseru/angular-springboot-crud)
with SQLite to replace H2, downgraded to Spring Boot __1.5.4__

### Build

download `node.js` of relevant vereison from `https://nodejs.org/download/`:

```cmd
curl -O https://nodejs.org/download/release/latest-v9.x/node-v9.11.2-win-x64.zip
```
```cmd
unzip.exe node-v9.11.2-win-x64.zip
path=%path%;%CD%\node-v9.11.2-win-x64
```
```cmd
mkdir -p src/main/resources/static
touch !$/.gitkeep

```
build manually:

```cmd
cd angular_app
call npm.cmd install
call npx.cmd ng build --configuration production
```
* add install operations to maven `pom.xml`:
``` cmd
mvn package
mvn spring-boot:run
```
if get error in runtime saving the user
```text
```
force downgrade to __1.8__


open 	`http://localhost:8084/`
add user

see the log

eanExporter        : Registering beans for JMX exposure on startup
2025-07-08 17:37:03.775  INFO 25544 --- [           main] s.b.c.e.t.TomcatEmbeddedServletContainer : Tomcat started on port(s): 8084 (http)
Hibernate: insert into user (id, fname, lname) values (null, ?, ?)
Hibernate: insert into user (id, fname, lname) values (null, ?, ?)
Hibernate: insert into user (id, fname, lname) values (null, ?, ?)	 		
### Note

if you see error running the tests 
```txt
java.lang.ClassNotFoundException: javax.xml.bind.JAXBException
```
your version of Java is too new:

```cmd
java -version
```
```text
openjdk version "11.0.12" 2021-07-20 LTS
```

need to patch `pom.xml`:
```xml
```
if using Java 11 or newer and migrating to Jakarta XML Binding, note the package name change from javax.xml.bind to jakarta.xml.bind
access app on
### See Also
  * [list](https://github.com/search?q=angular+2+language%3AJava&type=repositories&l=Java&s=updated&o=desc&p=35) of Java Angular 2 projects ordered by age, from 2023 or ealier
* https://stackoverflow.com/questions/43574426/how-to-resolve-java-lang-noclassdeffounderror-javax-xml-bind-jaxbexception


[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
