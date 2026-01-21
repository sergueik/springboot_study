### Usage
create a simple dummy Copybook () file 
```
       01  SAMPLE-REC.
           05 CUSTOMER-ID        PIC X(10).
           05 CUSTOMER-NAME      PIC X(20).
           05 ACCOUNT-NUMBER     PIC 9(9).
           05 BALANCE            PIC S9(7)V99 COMP-3.
```          
then run 

```
mvn package


```
and  generate the binary data. 
```
java -cp target\example.generator.jar;target\lib\* example.Generator -copybook example.cbl -out sample.bin -customerId ABC123 -name "JOHN DOE" -account 123456789 -balance 1050.75
```


> NOTE: in early revision data will *not* be __EBCDIC__ encoded

examine the file (no need to extract to json yet):
```sh
cat sample.bin
```
```text
ABC123    JOHN DOE            123456789\
```
extract to json:
```sh
java -jar build\cobol2json\target\cobolToJson-0.93.3.jar -cobol Example\cobol\example.cbl -fileOrganisation FixedWidth -font cp037 -input Example\in\sample.bin -output DTAR020.json
```
> NOTE: the data  will be broken becuase of __EBCDIC__
```json
{
  "SAMPLE-REC" : [ {
    "CUSTOMER-ID" : "┬á├ó├ñ┬æ\u0016┬ô┬Ç┬Ç┬Ç┬Ç",
    "CUSTOMER-NAME" : "┬ó|├º+┬Ç├á|├í┬Ç┬Ç┬Ç┬Ç┬Ç┬Ç┬Ç┬Ç┬Ç┬Ç┬Ç┬Ç",
    "BALANCE" : 1050.75
  } ]
}
```
### See Also:
 
 * [cb2xml](https://github.com/bmTas/cb2xml)
 * [JRecord](https://github.com/bmTas/JRecord)
 * [CobolToJson](https://github.com/bmTas/CobolToJson)
 * [Sourceforge download](https://sourceforge.net/projects/coboltojson/) convert cobol Data Files to JSON

---
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
