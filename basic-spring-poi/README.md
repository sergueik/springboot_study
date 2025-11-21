### Info

 * replica of the [springboot excel manipulation app project](https://github.com/T5750/poi) 
downgraded to older version `2.3.4.RELEASE` of `spring-boot-starter` 
and `poi` `4.1.1`
( - the latter requred changes from `cell.getCellType()` to `cell.getCellTypeEnum()`) 
and base alpine __3.9__ jre Docker image


### Usage

* local test
```sh
mvn spring-boot run
```

### Usage

* interact with in the browser `http://localhost:8080/poi`


The application has sample excel files in resource directory for reading and can generate Excel 2003 and 2007. 
It currently does not support merging excel files
### Container 

application can be placed in basic `openjdk:8-jre-alpine3.9` container
### NOTE

compile error
```txt
[ERROR] Failed to execute goal on project poi: 
Could not resolve dependencies for project example:poi:jar:0.4.0-SNAPSHOT: 
Failed to collect dependencies at org.apache.poi:poi:jar:4.1.1: 
Failed to read artifact descriptor for org.apache.poi:poi:jar:4.1.1: 
Could not transfer artifact org.apache.poi:poi:pom:4.1.1 from/to central 
(https://repo.maven.apache.org/maven2): sun.security.validator.ValidatorException: 
PKIX path building failed: sun.security.provider.certpath.SunCertPath
BuilderException: unable to find valid certification path to requested target
 ->
 [Help 1]
[ERROR]
```
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
