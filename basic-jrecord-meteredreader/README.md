### Info

```sh
mvn clean package
```

```sh
java -cp target\example.metered-reader.jar;target\lib\* example.Reader -inputfile example.bin -copybookfile example.cbl -debug true -maxcount 10 | c:\tools\jq.exe "."
```
```json
{
  "CUSTOMER-ID": "AAAAAAAAAA",
  "CUSTOMER-NAME": "AAAAAAAAAAAAAAAAAAAA",
  "ACCOUNT-NUMBER": "33376",
  "ACCOUNT-TYPE": "AA",
  "OPEN-DATE": "255479",
  "BALANCE": "0.00",
  "CREDIT-LIMIT": "0.00",
  "STATUS-CODE": "A",
  "LAST-ACTIVITY-DATE": "716164",
  "RESERVED-FLAG": "A"
}
```
```sh
java -cp target\example.metered-reader.jar;target\lib\* example.Reader -inputfile example.bin -copybookfile example.cbl -maxrows 10 | c:\tools\jq.exe "[.]" | jq ".[0:10]"
```
```sh
java -cp target\example.metered-reader.jar;target\lib\* example.Reader -inputfile example.bin -copybookfile example.cbl -maxrows 1000000 -debug true 1>nul
[inputfile, copybookfile, maxrows, debug]
Processed 1000000 records in 6720 ms

```
### See Also:

 
 * [cb2xml](https://github.com/bmTas/cb2xml)
 * [JRecord](https://github.com/bmTas/JRecord)
 * [CobolToJson](https://github.com/bmTas/CobolToJson)
 * [Sourceforge download](https://sourceforge.net/projects/coboltojson/) convert cobol Data Files to JSON
 * [Sourceforge download](https://sourceforge.net/projects/jrecord/files/JRecord/0.93.3/JRecord-0.93.3-src.zip/download) of JRecord jar bundle (old version)

---
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
