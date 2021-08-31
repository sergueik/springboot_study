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

Additional configuation required to make the `@Value` annotated class properties be available in test run. Currently receiving and catching `NullPointerException` in the controller when building the page with complex properties objects.
getting nothing in logging:
```java
```
is logging during test run
```sh
15:56:29.824 [main] INFO example.ExampleController - staticValue=null message=null
```

and real run
```
2021-08-31 16:01:40.776  INFO 17480 --- [nio-8080-exec-2] example.ExampleController                : staticValue=null message=message from properties file

```

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
