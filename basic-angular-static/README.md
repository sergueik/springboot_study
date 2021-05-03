### Info

This directory contains a replica of 
[angular code loaded from web jar](https://github.com/lsvidal/springboot-angular) and runs both MVC and REST controllers to host an Angular app in the static page pulling the data via `$http`.
### Testing

```sh
mvn clean spring-boot:run 
```
and open the `http://localhost:8080/` in the browser. 
will see in the logs:
```sh
Returning: {"results":[{"text":"Hello basic"}]}
```
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
