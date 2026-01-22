### Usage
#### Basic

`example.cbl`:
```text
       01  SAMPLE-REC.
           05 CUSTOMER-ID        PIC X(10).
           05 CUSTOMER-NAME      PIC X(20).
           05 ACCOUNT-NUMBER     PIC 9(9).
           05 BALANCE            PIC S9(7)V99 COMP-3.
```
generate the binary data based on copybook using __JRecord__ `0.93.2` *SourceForge release*:

```sh
java -cp target\example.generator.jar;target\lib\* example.Generator
```
```text
? EBCDIC row written to: sample.bin
```
`sample.bin`:
```
┴┬├±≥≤@@@@\
```
```cmd
copy /y sample.bin ..\basic-cobol2json-cb2xml-jrecord-build\Example\in
```
extact copybook fields from binary "batch" record using `JRecord`:
```sh
java -jar build\cobol2json\target\cobolToJson-0.93.3.jar -cobol Example\cobol\example.cbl -fileOrganisation FixedWidth -font cp037 -input Example\in\sample.bin -output example.json
```
`example.json`:
```json
{
  "SAMPLE-REC" : [ {
    "CUSTOMER-ID" : "ABC123",
    "BALANCE" : 1050.75
  } ]
}
```
>  NOTE: not all fields created in batch file

```java

        // Set fields by name (names must match the copybook)
        line.setField("CUSTOMER-ID", "ABC123");
        line.setField("NAME", "JOHN DOE");
        line.setField("ACCOUNT", new BigDecimal("123456789"));
        line.setField("BALANCE", new BigDecimal("1050.75"));

```
get recognized to `cobol2json`.
It was the effect of the type in the field name ; `ACCOUNT` should be `ACCOUNT_NUMBER`.
Fix and repeat, and observe:

```json
{
  "SAMPLE-REC" : [ {
    "CUSTOMER-ID" : "ABC123",
    "ACCOUNT-NUMBER" : 123456789,
    "BALANCE" : 1050.75
  } ]
}

```
#### Slightly more advanced


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
### Troubleshooting

in JRecord 0.93.3, the code is split across multiple JARs.
JRecord_Cobol.jar → present ✅

JRecord.jar → missing ❌

net.sf.JRecord.External.Definition is NOT in the COBOL-only JAR

it is difficult to find JRecord_Cobol

[200~Go to the JRecord SourceForge downloads page:
👉 https://sourceforge.net/projects/jrecord/files/0.93.3/
n Maven

There is no official Maven Central release for the COBOL JAR, so you will need to install it into your local Maven repo manually

### Using SourceForge Flavor of JRecord
```
unzip -l ~/Downloads/JRecord_0.93.2.zip
```
```
unzip -x ~/Downloads/JRecord_0.93.2.zip lib/JRecordJars2023_Feb_03.zip
```
```sh
unzip -l lib/JRecordJars2023_Feb_03.zip

```
```sh
unzip -x lib/JRecordJars2023_Feb_03.zip
```

```sh
mvn deploy:deploy-file -DgroupId=net.sf.jrecord -DartifactId=JRecord -Dversion=0.93.2 -Dpackaging=jar -Dfile=./JRecord.jar -Durl=file://$HOME/.m2/repository -DrepositoryId=local && mvn deploy:deploy-file -DgroupId=net.sf.jrecord -DartifactId=JRecordCodeGen -Dversion=0.93.2 -Dpackaging=jar -Dfile=./JRecordCodeGen.jar -Durl=file://$HOME/.

```
```sh
mkdir -p ./maven-local-repo && mvn deploy:deploy-file -DgroupId=net.sf.jrecord -DartifactId=JRecord -Dversion=0.93.2 -Dpackaging=jar -Dfile=./JRecord.jar -Durl=file://$PWD/maven-local-repo -DrepositoryId=local && mvn deploy:deploy-file -DgroupId=net.sf.jrecord -DartifactId=JRecordCodeGen -Dversion=0.93.2 -Dpackaging=jar -Dfile=./JRecordCodeGen.jar -Durl=file://$PWD/maven-local-repo -DrepositoryId=local && mvn deploy:deploy-file -DgroupId=net.sf.jrecord -DartifactId=cb2xml -Dversion=0.93.2 -Dpackaging=jar -Dfile=./cb2xml.jar -Durl=file://$PWD/maven-local-repo -DrepositoryId=local

```
```cmd
mvn deploy:deploy-file -DgroupId=net.sf.jrecord -DartifactId=JRecord -Dversion=0.93.2 -Dpackaging=jar -Dfile=JRecord.jar -Durl=file:///%USERPROFILE%/.m2/repository -DrepositoryId=local & mvn deploy:deploy-file -DgroupId=net.sf.jrecord -DartifactId=JRecordCodeGen -Dversion=0.93.2 -Dpackaging=jar -Dfile=JRecordCodeGen.jar -Durl=file:///%USERPROFILE%/.m2/repository -DrepositoryId=local & mvn deploy:deploy-file -DgroupId=net.sf.jrecord -DartifactId=cb2xml -Dversion=0.93.2 -Dpackaging=jar -Dfile=cb2xml.jar -Durl=file:///%USERPROFILE%/.m2/repository -DrepositoryId=local

```
```cmd

if not exist .\maven-local-repo mkdir .\maven-local-repo & mvn deploy:deploy-file -DgroupId=net.sf.jrecord -DartifactId=JRecord -Dversion=0.93.2 -Dpackaging=jar -Dfile=JRecord.jar -Durl=file:///%CD%/maven-local-repo -DrepositoryId=local & mvn deploy:deploy-file -DgroupId=net.sf.jrecord -DartifactId=JRecordCodeGen -Dversion=0.93.2 -Dpackaging=jar -Dfile=JRecordCodeGen.jar -Durl=file:///%CD%/maven-local-repo -DrepositoryId=local & mvn deploy:deploy-file -DgroupId=net.sf.jrecord -DartifactId=cb2xml -Dversion=0.93.2 -Dpackaging=jar -Dfile=cb2xml.jar -Durl=file:///%CD%/maven-local-repo -DrepositoryId=local

```
### See Also:
 
 * [cb2xml](https://github.com/bmTas/cb2xml)
 * [JRecord](https://github.com/bmTas/JRecord)
 * [CobolToJson](https://github.com/bmTas/CobolToJson)
 * [Sourceforge download](https://sourceforge.net/projects/coboltojson/) convert cobol Data Files to JSON

---
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
