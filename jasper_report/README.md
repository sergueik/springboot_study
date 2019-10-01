### Info

This directory contains a basic springboot hosted jasper reports runner project cloned from [](https://github.com/gauravbrills/jasperreportswithspringboot).
### Run application

Compile and start as a regular spring-boot appplication
```sh
mvn clean spring-boot:run
```
To verify it works, access application in Postman or curl like http://localhost:8080/report/{reportname}?format={format}&id={id} (id param are optional,you can add ur custom report params there).
