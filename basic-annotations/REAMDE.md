### Info
directory contains exercise code from Spring @Value Annotation Example [post](https://www.websparrow.org/spring/spring-value-annotation-example)
### Usage
```sh
curl http://localhost:8080/employee
```
```sh
Static variable=null<br/>
Fixed value=fixed value<br/>
Value Message=message from properties file <br/>
Property with Default value= default value<br/>
List values=[one, two, there]<br/>
Size of list=3<br/>
Object value={name=name, property 1=value 1, property 2=value 2}
```
### Note

`@Autowired` annotation required to make the `@Value` annotated class properties be available in test run. 
This is illustrated in `FirstControllerTest` receiving `NullPointerException`
in the controller when building the page with complex properties objects.
getting nothing in logging:
```java
```
is logging during test run
```sh
[INFO] Running example.controller.FirstTest
...
15:56:29.824 [main] INFO example.ExampleController -
staticValue=null
message=null
property with defaul value=null
```

and real run
```
2021-08-31 16:01:40.776  INFO 17480 --- [nio-8080-exec-2] example.ExampleController:
staticValue=null
message=message from properties file
property with defaul value= default value
```
The `SecondTest` and `ThirdTest` are added to show how it can be fixed
```text
[INFO] Running example.controller.SecondTest
2021-09-05 16:32:10.205  INFO 8426 --- [           main] example.controller.ExampleController: 
staticValue=null 
message=test message from annotation 
property with defaul value= default value
```

```text
[INFO] Running example.controller.ThirdTest
2021-09-05 16:37:46.350  INFO 8786 --- [           main] example.controller.ExampleController: 
staticValue=null 
message=test message from properties file 
property with defaul value= default value
```
### See Also
  * [how to configire test overriding a property file](https://www.google.com/search?q=angularjs%20check%20long%20running%20operation%20status)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
