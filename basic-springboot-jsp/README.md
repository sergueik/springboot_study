### Info

In this directory replica of a basic __Springboot jsp__ page with tests

### Usage
* real run
```sh
mvn -Dmaven.test.skip=true clean spring-boot:run
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
* update the code as needed to run with  latest HtlmUnit (signature  and location of the HtmlParser has changed between __2.32__ and __2.52.0__
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
### TODO
* fix TLD scan error - slows the test considerably
```text

2022-03-22 02:03:37.773  WARN 3690 --- [           main] o.a.tomcat.util.scan.StandardJarScanner  : Failed to scan [file:/home/sergueik/.m2/repository/xalan/xalan/2.7.2/xercesImpl.jar] from classloader hierarchy


2022-03-22 02:00:47.367  WARN 3440 --- [           main] o.a.tomcat.util.scan.StandardJarScanner  : Failed to scan [file:/home/sergueik/.m2/repository/xalan/serializer/2.7.2/xml-apis.jar] from classloader hierarchy

2022-03-22 02:03:37.823  WARN 3690 --- [           main] o.a.tomcat.util.scan.StandardJarScanner  : Failed to scan [file:/home/sergueik/.m2/repository/xalan/xalan/2.7.2/serializer.jar] from classloader hierarchy


```
* error with HtmlUnit  __2.50.0__,__2.60.0__:
```text
java.lang.IllegalStateException:
No script object associated with the Page. class: 'com.gargoylesoftware.htmlunit
.html.HtmlPage' url: 'http://localhost:8080/model' content: <!DOCTYPE html>

<html lang="en">
<head>
    <meta charset="UTF-8">
    <title>Hello</title>
</head>
<body>
  <div id="0">
    Hello World
  </div>
</body>
</html>
        at example.controller.AcceptanceTest.getHtmlPage(AcceptanceTest.java:150
```
### See Also

  * https://www.baeldung.com/integration-testing-in-spring -  with full
  * https://htmlunit.sourceforge.io/gettingStarted.html
  * https://htmlunit.sourceforge.io/faq.html
  * https://stackoverflow.com/questions/6136435/how-to-create-htmlunit-htmlpage-object-from-string
  * https://docs.spring.io/spring-framework/docs/current/reference/html/web.html

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
