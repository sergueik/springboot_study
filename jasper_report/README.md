### Info

This directory contains a basic springboot hosted jasper reports runner project cloned from [](https://github.com/gauravbrills/jasperreportswithspringboot).
### Run application

Compile and start as a regular spring-boot appplication
```sh
mvn clean spring-boot:run
```
To verify it works, access application in Postman or curl like 
`http://localhost:8080/report/{reportname}?format={format}&id={id}`. 
The `id` param are optional, one may add custom report params.

### See Also:

* [JasperReports overview](https://habr.com/ru/company/tinkoff/blog/461719/) (in Russian)