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
```sh
export FINAL_NAME=demo
mvn clean package
```
```sh
IMAGE_NAME=basic-spring-mvc 
docker build -t $IMAGE_NAME -f Dockerfile .
```
```sh
CONTAINER_NAME=basic-spring-mvc-app
docker run -d -p 8080:8080 --name $CONTAINER_NAME $IMAGE_NAME
```
open the tomcat manager page `http://$(hostname -f):8080/manager/html`
with admin:admin  and observe 
the __/demo__ one listed and running. Open it in the browser `http://$(hostname -f):8080/demo/`

### Cleanup
```sh
docker container rm -f  $CONTAINER_NAME
docker image rm -f $IMAGE_NAME
```
### Tomcat 8.5 
```sh
docker pull tomcat:8.5.27-jre8-alpine
```
```sh
mvn -Ptomcat85 clean package
```
```sh
IMAGE_NAME=basic-tomcat85-spring-mvn
docker build -t $IMAGE_NAME -f Dockerfile.tomcat85 .
```
```sh
CONTAINER_NAME=basic-tomcat85-spring-mvc-app
docker run -d -p 8080:8080 --name $CONTAINER_NAME $IMAGE_NAME

### See Also
  * [step by step](https://github.com/in28minutes/SpringIn28Minutes) Spring tutorial
  * [base Spring MVC based application project template](https://github.com/dev9com/sample-spring-webapp) with test etc, but with very old spring version
  * https://github.com/spring-projects/spring-mvc-showcase
  * https://stackoverflow.com/questions/14430122/how-to-define-conditional-properties-in-maven/14430203
  * Syntax of [referring to environment variables](https://www.baeldung.com/maven-env-variables) in `pom.xml`
  * Maven Properties schema [reference](https://books.sonatype.com/mvnref-book/reference/resource-filtering-sect-properties.html)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
