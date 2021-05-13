### Info

This directory contains a replica of 
[angular code loaded from web jar](https://github.com/lsvidal/springboot-angular) and runs MVC  page controller returning models (currently just `index`) and RestController controller returning pages and a static Angular app pulling the data onto the page via `$http`.
### Testing

```sh
mvn clean spring-boot:run 
```
and open the `http://localhost:8080/` in the browser. 
will see in the logs:
```sh
Returning: {"results":[{"text":"Hello basic"}]}
Returning: ["bob","carl","alice"]
```

The resources are :

```sh
META-INF/resources/webjars/angularjs/1.5.8/angular.js
META-INF/resources/webjars/bootstrap/3.3.7/css/bootstrap.min.css
META-INF/resources/webjars/jquery/3.2.1/jquery.js
```
there is a way to map different path, not used in this project
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
