### Info 

Basic Thymeleaf application showing debug page listing all accessible to Thyleaft objects 
 based on:
 https://stackoverflow.com/questions/31387526/list-all-available-model-attributes-in-thymeleaf
 see also:
 https://www.thymeleaf.org/doc/articles/springmvcaccessdata.html
 https://www.baeldung.com/spring-mvc-session-attributes
 see also:
 https://stackoverflow.com/questions/18791645/how-to-use-session-attributes-in-spring-mvc
### Usage:

```sh

mvn clean spring-boot:run
```
NOTE:

the session "data" is only updaated the first time. From application logs:

```text
2023-06-02 16:26:47.417  INFO 2196 --- [nio-8080-exec-1] example.controller.Thym
eleafController   : responseBody:
{"new":"10","old":""}
2023-06-02 16:27:02.147  INFO 2196 --- [nio-8080-exec-3] example.controller.Thym
eleafController   : Reading session attribute: 20
2023-06-02 16:27:02.149  INFO 2196 --- [nio-8080-exec-3] example.controller.Thym
eleafController   : responseBody:
{"new":"20","old":"10"}
2023-06-02 16:27:15.100  INFO 2196 --- [nio-8080-exec-5] example.controller.Thym
eleafController   : Reading session attribute: 30
2023-06-02 16:27:15.103  INFO 2196 --- [nio-8080-exec-5] example.controller.Thym
eleafController   : responseBody:
{"new":"30","old":"10"}
```

the same is shown in the browser or curl or postman


### TODO: 

see test
 https://github.com/spring-projects/spring-framework/blob/main/spring-test/src/test/java/org/springframework/test/web/servlet/samples/standalone/resultmatchers/SessionAttributeAssertionTests.java


### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
