### Info

```sh
mvn clean package
```

```sh
java -cp target\example.metered-reader.jar;target\lib\* example.Runner -inputfile example.bin -copybookfile example.cbl -debug true -maxcount 10 | c:\tools\jq.exe "."
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
java -cp target\example.metered-reader.jar;target\lib\* example.Runner -inputfile example.bin -copybookfile example.cbl -maxrows 10 | c:\tools\jq.exe "[.]" | jq ".[0:10]"
```
```sh
java -cp target\example.metered-reader.jar;target\lib\* example.Runner -inputfile example.bin -copybookfile example.cbl -maxrows 1000000 -debug true 1>nul
[inputfile, copybookfile, maxrows, debug]
Processed 1000000 records in 6720 ms

```

```cmd
pushd ..\basic-jrecord-dummydata
java -cp target\example.generator.jar;target\lib\* example.Generator  -outputfile example.bin -copybookfile example.cbl -maxrows 1000
copy  /y example.bin ..\basic-jrecord-meteredreader\
popd
```
```cmd
java -cp target\example.metered-reader.jar;target\lib\* example.Runner -debug true -benchmark true -inputfile example.bin -copybookfile example.cbl -maxrows 10
```
```text	

[inputfile, copybookfile, maxrows, debug, benchmark]
run with options: fork: 1 warmup: 1 measurement: 2 copybookFile: [example.cbl] inputFile: [example.bin] maxRows: [10]
ParseRecords copybook:example.cbl input:example.bin maxRows: 10
ParseRecords copybook:example.cbl input:example.bin maxRows: 10
{"CUSTOMER-ID":"AAAAAAAAAA","CUSTOMER-NAME":"7cb08291-3","ACCOUNT-NUMBER":"48787","ACCOUNT-TYPE":"AA","OPEN-DATE":"255479","BALANCE":"819.01","CREDIT-LIMIT":"0.00","STATUS-CODE":"A","LAST-ACTIVITY-DATE":"716164","RESERVED-FLAG":"A"}
{"CUSTOMER-ID":"AAAAAAAAAA","CUSTOMER-NAME":"551b20ac-7","ACCOUNT-NUMBER":"41218","ACCOUNT-TYPE":"AA","OPEN-DATE":"255479","BALANCE":"6355.15","CREDIT-LIMIT":"0.00","STATUS-CODE":"A","LAST-ACTIVITY-DATE":"716164","RESERVED-FLAG":"A"}
{"CUSTOMER-ID":"AAAAAAAAAA","CUSTOMER-NAME":"f02588c9-8","ACCOUNT-NUMBER":"74969","ACCOUNT-TYPE":"AA","OPEN-DATE":"255479","BALANCE":"8969.51","CREDIT-LIMIT":"0.00","STATUS-CODE":"A","LAST-ACTIVITY-DATE":"716164","RESERVED-FLAG":"A"}
{"CUSTOMER-ID":"AAAAAAAAAA","CUSTOMER-NAME":"2ad7e5c0-5","ACCOUNT-NUMBER":"98190","ACCOUNT-TYPE":"AA","OPEN-DATE":"255479","BALANCE":"9605.88","CREDIT-LIMIT":"0.00","STATUS-CODE":"A","LAST-ACTIVITY-DATE":"716164","RESERVED-FLAG":"A"}
{"CUSTOMER-ID":"AAAAAAAAAA","CUSTOMER-NAME":"0c60fe4b-5","ACCOUNT-NUMBER":"62902","ACCOUNT-TYPE":"AA","OPEN-DATE":"255479","BALANCE":"6058.16","CREDIT-LIMIT":"0.00","STATUS-CODE":"A","LAST-ACTIVITY-DATE":"716164","RESERVED-FLAG":"A"}
{"CUSTOMER-ID":"AAAAAAAAAA","CUSTOMER-NAME":"709b3264-9","ACCOUNT-NUMBER":"80016","ACCOUNT-TYPE":"AA","OPEN-DATE":"255479","BALANCE":"8437.64","CREDIT-LIMIT":"0.00","STATUS-CODE":"A","LAST-ACTIVITY-DATE":"716164","RESERVED-FLAG":"A"}
{"CUSTOMER-ID":"AAAAAAAAAA","CUSTOMER-NAME":"74474550-9","ACCOUNT-NUMBER":"64728","ACCOUNT-TYPE":"AA","OPEN-DATE":"255479","BALANCE":"7783.04","CREDIT-LIMIT":"0.00","STATUS-CODE":"A","LAST-ACTIVITY-DATE":"716164","RESERVED-FLAG":"A"}
{"CUSTOMER-ID":"AAAAAAAAAA","CUSTOMER-NAME":"66685a33-f","ACCOUNT-NUMBER":"61963","ACCOUNT-TYPE":"AA","OPEN-DATE":"255479","BALANCE":"6580.04","CREDIT-LIMIT":"0.00","STATUS-CODE":"A","LAST-ACTIVITY-DATE":"716164","RESERVED-FLAG":"A"}
{"CUSTOMER-ID":"AAAAAAAAAA","CUSTOMER-NAME":"9796f1b7-4","ACCOUNT-NUMBER":"93715","ACCOUNT-TYPE":"AA","OPEN-DATE":"255479","BALANCE":"5248.93","CREDIT-LIMIT":"0.00","STATUS-CODE":"A","LAST-ACTIVITY-DATE":"716164","RESERVED-FLAG":"A"}
{"CUSTOMER-ID":"AAAAAAAAAA","CUSTOMER-NAME":"f8fd2389-d","ACCOUNT-NUMBER":"15028","ACCOUNT-TYPE":"AA","OPEN-DATE":"255479","BALANCE":"9625.82","CREDIT-LIMIT":"0.00","STATUS-CODE":"A","LAST-ACTIVITY-DATE":"716164","RESERVED-FLAG":"A"}
Processed 10 records in 4 ms
...

```

### NOTE

So your output:

ParseRecords copybook:example.cbl ...
Processed 10 records in 6 ms
... hundreds of times


means:

__JMH___ is invoking your @Benchmark method in a loop to gather statistically meaningful timing data.

This is intentional and required for accuracy.

By default JMH uses time-based iterations, not count-based.

Each iteration runs for about 1 second (default):

### See Also:

 
 * [cb2xml](https://github.com/bmTas/cb2xml)
 * [JRecord](https://github.com/bmTas/JRecord)
 * [CobolToJson](https://github.com/bmTas/CobolToJson)
 * [Sourceforge download](https://sourceforge.net/projects/coboltojson/) convert cobol Data Files to JSON
 * [Sourceforge download](https://sourceforge.net/projects/jrecord/files/JRecord/0.93.3/JRecord-0.93.3-src.zip/download) of JRecord jar bundle (old version)

---
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
