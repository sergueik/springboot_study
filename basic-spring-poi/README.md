### Info

 * replica of the [springboot excel manipulation app project](https://github.com/T5750/poi) downgrated to less aggressively new versions of `spring-boot-starter` and `poi` ( - the latter requred changes from `cell.getCellType()` to `cell.getCellTypeEnum()`) and base alpine __3.9__ jre image


### Usage

* local test
```sh
mvn spring-boot run
```
* interact with in the browser `http://localhost:8080/poi`

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
