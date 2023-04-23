#### Info
basic JSP code with configuration done through `web.xml` and `applicationContext.xml`

* old  code, JDK 1.7, tomcat 7. Conversion to Tomcat 9, JDK 1.8 or 11 and [cargo](https://stackoverflow.com/questions/41326911/maven-plugin-for-tomcat-9) Maven plugin is Work in Progress

```sh
mvn tomcat7:run-war
```

observe it print

```text

[INFO] Webapp assembled
[INFO] Building war: ...\target\demo.war
[INFO] WEB-INF\web.xml already added, skipping
Running war on http://localhost:8080/demo
```

open the url `http://localhost:8080/demo` in the browser
### See Also

  * https://stackoverflow.com/questions/36963248/the-type-java-io-objectinputstream-cannot-be-resolved-it-is-indirectly-referenc
  * https://stackoverflow.com/questions/41326911/maven-plugin-for-tomcat-9

 
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
