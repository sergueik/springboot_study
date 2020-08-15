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
### See Also
  * [linking the thymeleaft page resources stored in local directory](https://stackoverflow.com/questions/29460618/inserting-an-image-from-local-directory-in-thymeleaf-spring-framework-with-mave)
  * [thymeleaf in a Spring MVC application](https://www.baeldung.com/thymeleaf-in-spring-mvc)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
