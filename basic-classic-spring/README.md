### Info


Basic Classic Spring MVC project cloned from [basic Spring MVC with AngularJS and Thymeleaf](https://github.com/xvitcoder/spring-mvc-angularjs)
converted to run on alpine tomcat base Docker image.
Note: this application features no `web.xml` - the base url configuration is done though  `finalName` in `pom.xml` and code `@RequestMapping` annotations .

### Test

* run locally
```sh
export FINAL_NAME=demo
```
or
```cmd
set FINAL_NAME=demo
```
```sh
mvn clean tomcat7:run
```

* test locally (best to be carried through a browser because of Angular an Bootstrap)
```sh
curl http://localhost:8080/demo
```
```html
<html lang="en" ng-app="AngularSpringApp">
<head>
    <meta charset="utf-8" />
    <title>Service App</title>
    <link rel="stylesheet" href="resources/css/app.css" />
    <link rel="stylesheet" href="resources/bootstrap/css/bootstrap.min.css" />
</head>
<body>
<div id="wrapper">
    <ul class="menu">
        <li><a href="#/cars">Cars</a></li>
        <li><a href="#/trains">Trains</a></li>
        <li><a href="#/railwaystations">Railway Station</a></li>
    </ul>
    <hr class="" />
    <div ng-view=""></div>
</div>

<script src="resources/js/lib/angular/angular.js"></script>
<script src="resources/js/app.js"></script>
<script src="resources/js/services.js"></script>
<script src="resources/js/controllers/RailwayStationController.js"></script>
<script src="resources/js/controllers/CarController.js"></script>
<script src="resources/js/controllers/TrainController.js"></script>
<script src="resources/js/filters.js"></script>
<script src="resources/js/directives.js"></script>
</body>
</html>

```
* run in container - work in progress

### See Also

 * [base Spring MVC based application project template](https://github.com/dev9com/sample-spring-webapp) with test etc, but with very old spring version
 * https://github.com/spring-projects/spring-mvc-showcase
 * https://stackoverflow.com/questions/14430122/how-to-define-conditional-properties-in-maven/14430203
 * Syntax of [referring to environment variables](https://www.baeldung.com/maven-env-variables) in `pom.xml`
 * Maven Properties schema [reference](https://books.sonatype.com/mvnref-book/reference/resource-filtering-sect-properties.html)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
