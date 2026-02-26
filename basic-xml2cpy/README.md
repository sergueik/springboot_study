### Info

### Usage

* launch the server
```sh
mvn spring-boot:test
```
```sh
curl -XPOST http://localhost:8085/copybook/to -H "Content-type: text/plain" -d @example1.cpy
```
```xml
<?xml version="1.0" encoding="UTF-8"?>
<copybook filename="cpy_6296129717620004970.tmp" dialect="Mainframe" cb2xml-format="2017">
    <item level="01" name="SAMPLE-REC" position="1" storage-length="69" display-length="77" display-position="1">
        <item level="05" name="CUSTOMER-ID" picture="X(10)" position="1" storage-length="10" display-length="10" display-position="1"/>
        <item level="05" name="CUSTOMER-NAME" picture="X(20)" position="11" storage-length="20" display-length="20" display-position="11"/>
        <item level="05" name="ACCOUNT-NUMBER" picture="9(9)" position="31" storage-length="9" display-length="9" numeric="COBOL_NUMERIC" display-position="31"/>
        <item level="05" name="ACCOUNT-TYPE" picture="X(2)" position="40" storage-length="2" display-length="2" display-position="40"/>
        <item level="05" name="OPEN-DATE" picture="9(8)" position="42" storage-length="8" display-length="8" numeric="COBOL_NUMERIC" display-position="42"/>
        <item level="05" name="BALANCE" picture="S9(7)V99" usage="computational-3" position="50" storage-length="5" display-length="9" scale="2" numeric="COBOL_NUMERIC" display-position="50"/>
        <item level="05" name="CREDIT-LIMIT" picture="S9(7)V99" usage="computational-3" position="55" storage-length="5" display-length="9" scale="2" numeric="COBOL_NUMERIC" display-position="59"/>
        <item level="05" name="STATUS-CODE" picture="X(1)" position="60" storage-length="1" display-length="1" display-position="68"/>
        <item level="05" name="LAST-ACTIVITY-DATE" picture="9(8)" position="61" storage-length="8" display-length="8" numeric="COBOL_NUMERIC" display-position="69"/>
        <item level="05" name="RESERVED-FLAG" picture="X(1)" position="69" storage-length="1" display-length="1" display-position="77"/>
    </item>
</copybook>
```
```sh
curl -XPOST http://localhost:8085/copybook/to -H "Content-type: text/plain" -d @example2.cpy
```
```
[Line Number = 1, Column = 16] expecting: '.'
```
```sh
```
curl -XPOST http://localhost:8085/copybook/from -H "Content-type: application/xml" -d @example1.xml
```

```text
01 SAMPLE-REC
05 CUSTOMER-ID PIC X(10)
05 CUSTOMER-NAME PIC X(20)
05 ACCOUNT-NUMBER PIC 9(9)
05 ACCOUNT-TYPE PIC X(2)
05 OPEN-DATE PIC 9(8)
05 BALANCE PIC S9(7)V99 USAGE computational-3
05 CREDIT-LIMIT PIC S9(7)V99 USAGE computational-3
05 STATUS-CODE PIC X(1)
05 LAST-ACTIVITY-DATE PIC 9(8)
05 RESERVED-FLAG PIC X(1)
```
< NOTE: may have the XML header -fixing is WIP:
```xml
<?xml version="1.0" encoding="UTF-8"?>
```
### Options
```sh
curl -XPOST 'http://localhost:8085/copybook/to?indent=true&format=FREE' -H "Content-Type: text/plain" -d @example1.cpy
```
```xml
<?xml version="1.0" encoding="UTF-8"?>
<copybook filename="cpy_7948257517072821333.tmp" dialect="Mainframe" cb2xml-format="2017">
  <item level="01" name="SAMPLE-REC" position="1" storage-length="69" display-length="77" display-position="1">
    <item level="05" name="CUSTOMER-ID" picture="X(10)" position="1" storage-length="10" display-length="10" display-position="1"/>
    <item level="05" name="CUSTOMER-NAME" picture="X(20)" position="11" storage-length="20" display-length="20" display-position="11"/>
    <item level="05" name="ACCOUNT-NUMBER" picture="9(9)" position="31" storage-length="9" display-length="9" numeric="COBOL_NUMERIC" display-position="31"/>
    <item level="05" name="ACCOUNT-TYPE" picture="X(2)" position="40" storage-length="2" display-length="2" display-position="40"/>
    <item level="05" name="OPEN-DATE" picture="9(8)" position="42" storage-length="8" display-length="8" numeric="COBOL_NUMERIC" display-position="42"/>
    <item level="05" name="BALANCE" picture="S9(7)V99" usage="computational-3" position="50" storage-length="5" display-length="9" scale="2" numeric="COBOL_NUMERIC" display-position="50"/>
    <item level="05" name="CREDIT-LIMIT" picture="S9(7)V99" usage="computational-3" position="55" storage-length="5" display-length="9" scale="2" numeric="COBOL_NUMERIC" display-position="59"/>
    <item level="05" name="STATUS-CODE" picture="X(1)" position="60" storage-length="1" display-length="1" display-position="68"/>
    <item level="05" name="LAST-ACTIVITY-DATE" picture="9(8)" position="61" storage-length="8" display-length="8" numeric="COBOL_NUMERIC" display-position="69"/>
    <item level="05" name="RESERVED-FLAG" picture="X(1)" position="69" storage-length="1" display-length="1" display-position="77"/>
  </item>
</copybook>
```
```sh
curl -XPOST 'http://localhost:8085/copybook/from?areaAWidth=6&nameStartCol=8' -H "Content-Type: application/xml" -d @example1.xml
```
### See Also:

 
 * [cb2xml](https://github.com/bmTas/cb2xml)
 * [JRecord](https://github.com/bmTas/JRecord)
 * [CobolToJson](https://github.com/bmTas/CobolToJson)
 * [Sourceforge download](https://sourceforge.net/projects/coboltojson/) convert cobol Data Files to JSON
 * [Sourceforge download](https://sourceforge.net/projects/jrecord/files/JRecord/0.93.3/JRecord-0.93.3-src.zip/download) of JRecord jar bundle (old version)
 * [sourceforge project](https://sourceforge.net/projects/jrecord/) 
 * [JRecord Wiki](https://sourceforge.net/p/jrecord/wiki/Home/)
 * [JRecord Discussion](https://sourceforge.net/p/jrecord/discussion/)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
