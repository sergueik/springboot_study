### Info


Basic Classic Spring MVC project cloned from [basic Spring MVC with AngularJS and Thymeleaf](https://github.com/xvitcoder/spring-mvc-angularjs)
converted to run on alpine tomcat base Docker image.
Note: this application features no `web.xml` - the base url configuration is done though  `finalName` in `pom.xml` and code `@RequestMapping` annotations .

### Test

* run locally
```sh
mvn clean tomcat7:run
```

* test locally (needs to be carried through a browser because of Angular)
```sh
curl http://localhost:8085/basic
Hello basic
```
* run in container - work in progress

### See Also

 * [base Spring MVC based application project template](https://github.com/dev9com/sample-spring-webapp)  with test etc, but with very old spring version
 * https://github.com/spring-projects/spring-mvc-showcase

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
