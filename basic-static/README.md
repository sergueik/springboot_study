### Info

Basic Springboot application hosting static page with a Angular JS  applcation, with unit test on HTTP status and page content validations added

### Testing
```sh
mvn  -Ddebug=true test
```
`debug` flag will trigger printig additional debugging information

### Running
```sh
mvn clean -Dmaven.test.skip=true spring-boot:run
```
followed by

```sh
curl http://localhost:8080
```

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
