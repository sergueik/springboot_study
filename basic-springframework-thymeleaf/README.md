### Info

replica of skeleton project [Hello World Thymeleaf and Spring example](https://github.com/jmiguelsamper/thymeleaf3-spring-helloworld) updated to later versions of thymeleaf and spring framedowk

### Usage
```sh
mvn package
mvn tomcat7:run
```
this will log to console:
```text
[INFO] Running war on http://localhost:8080/thymeleaf3-spring-helloworld
```
```
curl -s http://localhost:8080/thymeleaf3-spring-helloworld/
```
```html
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml">
        <head>
                <title>Thymeleaf 3 + Spring 4 example</title>
        <meta charset="utf-8" />
        </head>
        <body>
        <h1>Thymeleaf 3 + Spring 4 example</h1>
        <p>
            Hello <span>World</span>!!!
        </p>
        </body>
</html>
```
