### Usage

* create Cobol Copybook `example.cbl`:
```text
       01  SAMPLE-REC.
           05 CUSTOMER-ID        PIC X(10).
           05 CUSTOMER-NAME      PIC X(20).
           05 ACCOUNT-NUMBER     PIC 9(9).
           05 BALANCE            PIC S9(7)V99 COMP-3.
```
* generate the binary data 

```cmd
java -cp target\example.generator.jar;target\lib\* example.Generator -balance 1234.56 -name "Jimmy Smith" -outputfile example.bin -copybookfile example.cbl  -accountnumber 124356879
```
or
```sh
java -cp target/example.generator.jar:target/lib/* example.Generator -balance 1234.56 -name "Jimmy Smith" -outputfile example.bin -copybookfile example.cbl -accountnumber 124356879
```
* examine the `example.bin`:
```text
┴┬├±≥≤@@@@±≥⌠≤⌡÷°≈∙#E\
```
* extract the copybook data from binary record using Cobol2JSON (JRecord):

```cmd
```
```cmd
copy /y example.bin ..\basic-cobol2json-cb2xml-jrecord-build\Example\in
copy /y example.cobol ..\basic-cobol2json-cb2xml-jrecord-build\Example\cobol
```
or
```sh
cp example.cbl ../basic-cobol2json-cb2xml-jrecord-build/Example/cobol
cp example.bin ../basic-cobol2json-cb2xml-jrecord-build/Example/in
```
```cmd
pushd ..\basic-cobol2json-cb2xml-jrecord-build
```
```sh
java -jar build\cobol2json\target\cobolToJson-0.93.3.jar -cobol Example\cobol\example.cbl -fileOrganisation FixedWidth -font cp037 -input Example\in\example.bin -output example.json
```
examine the `example.json`:
```cmd
jq.exe "."  <  example.json
```
```json
{
  "SAMPLE-REC": [
    {
      "CUSTOMER-ID": "ABC123",
      "CUSTOMER-NAME": "JIMMY SMITH",
      "ACCOUNT-NUMBER": 124356879,
      "BALANCE": 1234.55
    }
  ]
}
```

### Build

find where is the dependency
```sh
find .. -iname 'cb2xml*jar'
```
```text
../basic-cobol2json-cb2xml-jrecord-build/build/m2/net/sf/cb2xml/1.01.08/cb2xml-1.01.08.jar
../basic-cobol2json-cb2xml-jrecord-build/build/cb2xml/target/cb2xml.jar
```
```sh
export MAVEN_LOCAL_REPO=$(pwd)/../basic-cobol2json-cb2xml-jrecord-build/build/m2
```
and then build
### See Also:
 
 * [cb2xml](https://github.com/bmTas/cb2xml)
 * [JRecord](https://github.com/bmTas/JRecord)
 * [CobolToJson](https://github.com/bmTas/CobolToJson)
 * [Sourceforge download](https://sourceforge.net/projects/coboltojson/) convert cobol Data Files to JSON
 * [Sourceforge download](https://sourceforge.net/projects/jrecord/files/JRecord/0.93.3/JRecord-0.93.3-src.zip/download) of JRecord jar bundle (old version)

---
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
