### Info

this directory contains a replica of
[fastxml/fastxml](https://github.com/fastxml/fastxml)
repository: *simple, high-performance, small memory footprint, pull(stream)-based standalone XML parser*
combined with its integration test example [project](https://github.com/fastxml/fastxml-example)
and tests from [anther XML pull parser repository](https://github.com/codegym-ua/Parser) (to be removed)

### Note 

the jar depenency reference 


```xml
<dependency>
    <groupId>com.github.fastxml</groupId>
    <artifactId>fastxml</artifactId>
    <version>1.0.0</version>
</dependency>
```
no longer seems to be available in maven central - it possibly was not published at all:

### Usage

* to run the original unit tests run
```sh
mvn test
```

* to test building a SOAP object from WSDL run
```sh
mvn integration-test
```
### See Also

  * [quick Introduction to XMLPULL V1 API](http://www.xmlpull.org/v1/download/unpacked/doc/quick_intro.html)
  * [xmlpull](https://mvnrepository.com/artifact/xmlpull/xmlpull)



### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
