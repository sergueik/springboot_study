### Info

Basic Springboot application hosting static page rendered by thymeleaf spring framework with a Angular JS and Bootstrap loaded, and few static resources loaded with unit test on HTTP status and page content validations (partially working) dded

### Testing
```sh
mvn  -Ddebug=true test
```
`debug` flag will trigger printig additional debugging information

### Running
```sh
mvn clean -Dmaven.test.skip=true spring-boot:run
```
followed by

```sh
curl http://localhost:8080
```

### Customize the mapping
```sh
cp src/main/resources/application.properties .
sed -i 's|application=.*$|application=test|' application.properties
mvn -Dspring.config.location=$(pwd)\application.properties spring-boot:run
```
NOTE: the attempt to directly set the propery on the command line without defaut(`src/main/resources/application.propetries`) 

```sh
mvn -Dapplication=test clean spring-boot:run
```
occasioally does not work:
```sh
Invocation of init method failed; 
nested exception is java.lang.IllegalStateException: 
Invalid mapping on handler class [example.controllers.DesignTacoController]: public java.lang.String example.controllers.DesignTacoController.showDesignForm(org.springframework.ui.Model)
```
### See Also
  * [linking the thymeleaft page resources stored in local directory](https://stackoverflow.com/questions/29460618/inserting-an-image-from-local-directory-in-thymeleaf-spring-framework-with-mave)
  * [thymeleaf in a Spring MVC application](https://www.baeldung.com/thymeleaf-in-spring-mvc)
  * [properties](https://www.baeldung.com/properties-with-spring) with Spring and Spring Boot
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
