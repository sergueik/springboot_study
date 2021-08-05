### Info

In this directory replica of a basic __Springboot jsp__ page with tests

### Usage
* real run
```sh
mvn -Dmaven.test.ski p=true clean spring-boot:run
```
* test run (like acceptance test)
```sh
mvn test
```
* test run illustrating effect of initializing too early
```sh
BADTEST=true mvn test
```

this will show
```text
org.springframework.web.client.ResourceAccessException: I/O error on GET request for "http://localhost:8085/": Connection refused (Connection refused);
nested exception is java.net.ConnectException: Connection refused (Connection refused)
	at example.controller.BrokenAcceptanceTest.test1(BrokenAcceptanceTest.java:79)
...
INFO] Results:
[INFO]
[ERROR] Errors:
[ERROR]   BrokenAcceptanceTest.test1:79 » ResourceAccess I/O error on GET request for "h...
```

### TODO
* add a Junit4 variant
* add `WebConfig` class
to prevent erors showing when enablied `HelloControllerTest`
```sh
DEBUG org.springframework.test.web.servlet.setup.StandaloneMockMvcBuilder$StaticRequestMappingHandlerMapping - Looking up handler method for path /
DEBUG org.springframework.test.web.servlet.setup.StandaloneMockMvcBuilder$StaticRequestMappingHandlerMapping -
Did not find handler method for [/]
WARN org.springframework.web.servlet.PageNotFound - No mapping found for HTTP request with URI [/] in DispatcherServlet with name ''
```
* fix the content to prevent runtime and test time error
```java
{
  "timestamp": 1628111654428,
  "status": 500,
  "error": "Internal Server Error",
  "exception": "javax.servlet.ServletException",
  "message": "Circular view path [/hello.jsp]: would dispatch back to the current handler URL [/hello.jsp] again. Check your ViewResolver setup! (Hint: This may be the result of an unspecified view, due to default view name generation.)",
  "path": "/hello"
}
```
when controller returns the view with the same name as route:
```java

public class HelloController {
	@RequestMapping("/hello")
	public String hello(Model model,
			@RequestParam(value = "name", required = false, defaultValue = "World") String name) {
		model.addAttribute("name", name);
		return "hello";
	}
}
```

The same code fails under Junit4 with `java.net.ConnectException Connection refused` error (broken version saved in commit [555b8f9](https://github.com/sergueik/springboot_study/commit/555b8f9c33b550313214b5ea4420ec78c31007a6))
```sh

```
### See Also

  * https://www.baeldung.com/integration-testing-in-spring -  with full
  * https://htmlunit.sourceforge.io/gettingStarted.html
  * https://htmlunit.sourceforge.io/faq.html
  * https://stackoverflow.com/questions/6136435/how-to-create-htmlunit-htmlpage-object-from-string
