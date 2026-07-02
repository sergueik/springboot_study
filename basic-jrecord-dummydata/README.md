### Usage

* create Cobol Copybook `example.cbl`:
```text
       01 SAMPLE-REC.
          05 CUSTOMER-ID            PIC X(10).
          05 CUSTOMER-NAME          PIC X(20).
          05 ACCOUNT-NUMBER         PIC 9(9).
          05 ACCOUNT-TYPE           PIC X(2).
          05 OPEN-DATE              PIC 9(8).
          05 BALANCE                PIC S9(7)V99 COMP-3.
          05 CREDIT-LIMIT           PIC S9(7)V99 COMP-3.
          05 STATUS-CODE            PIC X(1).
          05 LAST-ACTIVITY-DATE     PIC 9(8).
          05 RESERVED-FLAG          PIC X(1).
```
#### Build Using Vendor Sourced Versions

* find where is the dependency
```sh
find .. -iname 'cb2xml*jar'
```
```text
../basic-cobol2json-cb2xml-jrecord-build/build/m2/net/sf/cb2xml/1.01.08/cb2xml-1.01.08.jar
../basic-cobol2json-cb2xml-jrecord-build/build/cb2xml/target/cb2xml.jar
```
* point maven to it
```sh
export MAVEN_LOCAL_REPO=$(pwd)/../basic-cobol2json-cb2xml-jrecord-build/build/m2
```
and then build

> NOTE: using minimalistic plain map based `parseArgs()` does not understand flags - one has to pass true/false value for every "flag" arg:

```cmd
java -cp target\example.generator.jar;target\lib\* example.Runner  -outputfile example.bin -copybookfile example.cbl -debug true -maxrows 100000
```
instead of `commandline-parser`

```cmd
java -cp target\example.generator.jar;target\lib\* example.Generator  -outputfile example.bin -copybookfile example.cbl -debug
```
this will print debug level messages:
```text
[copybookfile, debug, outputfile, parse]
Create COBOL IO builder for example.cbl
Parse example.cbl
WARN: skipping group or unsupported field: SAMPLE-REC
EBCDIC row written to: example.bin
```

including the reference Copybook-pojo object JSON printed to console when `debug` is provided:
```JSON
[
  {
    "name": "CUSTOMER-ID",
    "type": "ALPHA",
    "intDigits": 10,
    "fracDigits": 0,
    "signed": false,
    "comp3": false,
    "level": 5
  },
  {
    "name": "CUSTOMER-NAME",
    "type": "ALPHA",
    "intDigits": 20,
    "fracDigits": 0,
    "signed": false,
    "comp3": false,
    "level": 5
  },
  {
    "name": "ACCOUNT-NUMBER",
    "type": "NUMERIC",
    "intDigits": 9,
    "fracDigits": 0,
    "signed": false,
    "comp3": false,
    "level": 5
  },
  {
    "name": "ACCOUNT-TYPE",
    "type": "ALPHA",
    "intDigits": 2,
    "fracDigits": 0,
    "signed": false,
    "comp3": false,
    "level": 5
  },
  {
    "name": "OPEN-DATE",
    "type": "NUMERIC",
    "intDigits": 8,
    "fracDigits": 0,
    "signed": false,
    "comp3": false,
    "level": 5
  },
  {
    "name": "BALANCE",
    "type": "NUMERIC",
    "intDigits": 7,
    "fracDigits": 9,
    "signed": true,
    "comp3": true,
    "level": 5
  },
  {
    "name": "CREDIT-LIMIT",
    "type": "NUMERIC",
    "intDigits": 7,
    "fracDigits": 9,
    "signed": true,
    "comp3": true,
    "level": 5
  },
  {
    "name": "STATUS-CODE",
    "type": "ALPHA",
    "intDigits": 1,
    "fracDigits": 0,
    "signed": false,
    "comp3": false,
    "level": 5
  },
  {
    "name": "LAST-ACTIVITY-DATE",
    "type": "NUMERIC",
    "intDigits": 8,
    "fracDigits": 0,
    "signed": false,
    "comp3": false,
    "level": 5
  },
  {
    "name": "RESERVED-FLAG",
    "type": "ALPHA",
    "intDigits": 1,
    "fracDigits": 0,
    "signed": false,
    "comp3": false,
    "level": 5
  }
]
```

and the actual dummy record:
```JSON
{
  "SAMPLE-REC": [
    {
      "CUSTOMER-ID": "AAAAAAAAAA",
      "CUSTOMER-NAME": "AAAAAAAAAAAAAAAAAAAA",
      "ACCOUNT-NUMBER": 33376,
      "ACCOUNT-TYPE": "AA",
      "OPEN-DATE": 255479,
      "BALANCE": 0.000783380,
      "CREDIT-LIMIT": 0.000197023,
      "STATUS-CODE": "A",
      "LAST-ACTIVITY-DATE": 716164,
      "RESERVED-FLAG": "A"
    }
  ]
}
```

* save the second JSON (`temp.json`), flatten the JSON output as `expected.json`:

```sh
jq ".[] | .[0]" temp.json > expected.json
```


examine the binary data: format is indeed in EBCDIC
`example.bin`:
```text
┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴≡≡≡≡≤≤≤≈÷┴┴≡≡≥⌡⌡⌠≈∙

                                                 ┴≡≡≈±÷±÷⌠┴
```
#### Verify
generate the JSON from binary data:

```cmd
copy /y example.bin ..\basic-cobol2json-cb2xml-jrecord-build\Example\in
copy /y example.cbl ..\basic-cobol2json-cb2xml-jrecord-build\Example\cobol
```
```
pushd ..\basic-cobol2json-cb2xml-jrecord-build
```
```powershell
java -jar build\cobol2json\target\cobolToJson-0.93.3.jar -cobol Example\cobol\example.cbl -fileOrganisation FixedWidth -font cp037 -input Example\in\example.bin -output example.json
```
```sh
jq.exe "." < example.json
```
```json
{
  "SAMPLE-REC": [
    {
      "CUSTOMER-ID": "AAAAAAAAAA",
      "CUSTOMER-NAME": "AAAAAAAAAAAAAAAAAAAA",
      "ACCOUNT-NUMBER": 33376,
      "ACCOUNT-TYPE": "AA",
      "OPEN-DATE": 255479,
      "BALANCE": 0.00,
      "CREDIT-LIMIT": 0.00,
      "STATUS-CODE": "A",
      "LAST-ACTIVITY-DATE": 716164,
      "RESERVED-FLAG": "A"
    }
  ]
}
```
*  compare

```sh
jq -S . expected.json > expected.norm.json
jq -S . example.json > actual.norm.json
```

```text
 Directory of C:\developer\sergueik\springboot_study\basic-jrecord-dummydata

01/23/2026  04:43 PM               298 expected.norm.json
               1 File(s)            298 bytes

 Directory of C:\developer\sergueik\springboot_study\basic-cobol2json-cb2xml-jrecord-build

01/23/2026  04:42 PM               362 actual.norm.json
```
```sh
diff -w expected.norm.json ..\basic-cobol2json-cb2xml-jrecord-build\actual.norm.json
```
```text
1a2,3
>   "SAMPLE-REC": [
>     {
4,5c6,7
<   "BALANCE": 0.000783380,
<   "CREDIT-LIMIT": 0.000197023,
---
>       "BALANCE": 0.00,
>       "CREDIT-LIMIT": 0.00,
11a14,15
>     }
>   ]
```

### Generate Test Data

```powershell
format-Hex -Path "Example\in\example.bin"
```
```text
           Path: Example\in\example.bin

           00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F

00000000   C1 C1 C1 C1 C1 C1 C1 C1 C1 C1 C1 C1 C1 C1 C1 C1  ÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁ
00000010   C1 C1 C1 C1 C1 C1 C1 C1 C1 C1 C1 C1 C1 C1 F0 F0  ÁÁÁÁÁÁÁÁÁÁÁÁÁÁðð
00000020   F0 F0 F3 F3 F3 F7 F6 C1 C1 F0 F0 F2 F5 F5 F4 F7  ððóóó÷öÁÁððòõõô÷
00000030   F9 00 00 00 00 0C 00 00 00 00 0C C1 F0 F0 F7 F1  ù..........Áðð÷ñ
00000040   F6 F1 F6 F4 C1                                   öñöôÁ
```
```powershell
$filePath =  (resolve-path -path "Example\in\example.bin" ).path
[Object[]]$bytes = get-content -path $filepath -encoding Byte -readcount 0
$hexString = [System.BitConverter]::ToString($bytes).Replace('-', '')
write-output $hexString
 ```
```text
C1C1C1C1C1C1C1C1C1C1C1C1C1C1C1C1C1C1C1C1C1C1C1C1C1C1C1C1C1C1F0F0F0F0F3F3F3F7F6C1C1F0F0F2F5F5F4F7F9000000000C000000000CC1F0F0F7F1F6F1F6F4C1
```

```powershell
$filepath =  (resolve-path -path "Example\in\example.bin" ).path
[Object[]]$bytes = get-content -path $filepath -encoding Byte -readcount 0
$hexString = ($bytes | foreach-object { $_.ToString('X2') }) -join ''
write-output $hexString
```

### Running the test
```cmd
mvn test
```
```text
Running command:
[c:\java\jdk-11.0.12\bin\java, -jar, basic-cobol2json-cb2xml-jrecord-build\build\cobol2json\target\cobolToJson-0.93.3.jar, -cobol, AppData\Local\Temp\coboltest8287271525148015959\copybook.cpy, -fileOrganisation, FixedWidth, -font, cp037, -input, AppData\Local\Temp\coboltest8287271525148015959\input.bin, -output, AppData\Local\Temp\coboltest8287271525148015959\output.json]
...
Verified value:  $.SAMPLE-REC[0].CREDIT-LIMIT = 0.0
Verified value:  $.SAMPLE-REC[0].ACCOUNT-NUMBER = 33376
Verified value:  $.SAMPLE-REC[0].STATUS-CODE = A
Verified value:  $.SAMPLE-REC[0].LAST-ACTIVITY-DATE = 716164
Verified value:  $.SAMPLE-REC[0].BALANCE = 0.0
Verified value:  $.SAMPLE-REC[0].OPEN-DATE = 255479
Verified value:  $.SAMPLE-REC[0].ACCOUNT-TYPE = AA
Verified value:  $.SAMPLE-REC[0].RESERVED-FLAG = A
Verified value:  $.SAMPLE-REC[0].CUSTOMER-ID = AAAAAAAAAA
Verified value:  $.SAMPLE-REC[0].CUSTOMER-NAME = AAAAAAAAAAAAAAAAAAAA
...
Expected exception caught: No results for path: $['SAMPLE-REC'][0]['STATUS_CODE']
[INFO] Tests run: 11, Failures: 0, Errors: 0, Skipped: 0, Time elapsed: 14.278 s - in example.IntegrationTest
```
### Note



The test validates copybook binary record field JSON extraction accuracy without assuming or depending on how the system under test constructs JSON from the copybook and binary input thus
ensuring consistent verification regardless of internal JSON-building logic.

Since we already have a reliable Cobol Copybook-driven, JRecord-compatible binary data generator, constructing robust tests for specific display, numeric, and even more complex data types is straightforward and scalable

#### Why There are Two Tests

While the generated JSON is fully valid, asserting multiple JSON paths in a single test `test2` resulted in random failures due to path-specific type and formatting differences; testing one path per test ensures deterministic and fully reliable validation of all COBOL copybook data types:
in repeated runs will observe:

```text
 Expected exception caught: No results for path: $['SAMPLE-REC'][0]['CREDIT_LIMIT']
 Expected exception caught: No results for path: $['SAMPLE-REC'][0]['LAST_ACTIVITY_DATE']
 Expected exception caught: No results for path: $['SAMPLE-REC'][0]['RESERVED_FLAG']
 Expected exception caught: No results for path: $['SAMPLE-REC'][0]['OPEN_DATE'] 
 Expected exception caught: No results for path: $['SAMPLE-REC'][0]['STATUS_CODE']	

```
> NOTE, sometimes the `test2` will fail in assertThat(actual, is(expected)) throwing `AssertionError`, which is not a subclass of `Exception` in Java — it’s a subclass of `Error`

### Troubleshooting

```txt
[ERROR] error reading C:\Users\kouzm\.m2\repository\net\sf\jrecord\0.90.2\jrecord-0.90.2.jar; zip END header not found
```
```sh
 Directory of C:\Users\kouzm\.m2\repository\net\sf\jrecord\0.90.2

06/23/2026  04:28 PM            25,372 jrecord-0.90.2.jar
               1 File(s)         25,372 bytes
               0 Dir(s)  106,373,591,040 bytes free
```
```sh
unzip -ql C:\Users\kouzm\.m2\repository\net\sf\jrecord\0.90.2\jrecord-0.90.2.jar
```
```text
[C:\Users\kouzm\.m2\repository\net\sf\jrecord\0.90.2\jrecord-0.90.2.jar]
  End-of-central-directory signature not found.  Either this file is not
  a zipfile, or it constitutes one disk of a multi-part archive.  In the
  latter case the central directory and zipfile comment will be found on
  the last disk(s) of this archive.

```
```cmd
set MAVEN_LOCAL_REPO=%CD%\..\basic-cobol2json-cb2xml-jrecord-build\build\m2 Directory of C:\Users\kouzm\.m2\repository\net\sf\jrecord\0.90.2

```
### Example css selecrtor style:
```text
$..[?(@.pic=="S9(7)V99")]
    → annotation: "financial numeric field"
    → annotation: "COMP-3 encoded"
    → validator: decimal-range
    → formatter: accounting-style
```
### TODO

### Deterministic Record Boundary Fault-Injection Test Harness

#### Overview

This project is a small deterministic data generation and fault-injection harness designed to evaluate parser behavior under mixed-quality input and controlled stream interruption conditions.

The primary goal is not bulk data validation, but precise observation of **boundary behavior in record-oriented parsers** (e.g., COBOL copybook-based systems such as jRecord).

---

#### Core Concept

The system generates sequential records with explicit row indexing and introduces controlled variation in data quality and structure to observe how downstream systems handle:

- partial record streams
- boundary corruption
- transition between valid and invalid segments
- stream truncation and mid-record interruption

The central hypothesis is that parser behavior is determined primarily by **failure boundary semantics**, not bulk input correctness.

---

#### Data Model

The dataset is composed of three logical regions:

##### 1. Bulk Region (Rows 1–99)
- Sequentially generated records
- May contain lower-fidelity or noisy data
- Purpose: simulate realistic baseline load conditions
- Row indexing is strictly preserved for deterministic traceability

##### 2. Boundary Anchor Region (Rows 100–101)
- High-fidelity or strictly schema-compliant records
- Either:
  - generated with stricter validation rules, or
  - injected as preformatted JSON payloads
- Purpose: create a known-valid structural reference point

##### 3. Failure Injection Region (optional)
- Stream is intentionally truncated or corrupted:
  - mid-record cut
  - byte-level interruption
  - or partial transmission simulation
- Purpose: simulate real-world transport or ingestion failure

---

#### Key Mechanism: Row Index Tracking

A deterministic row counter is maintained during generation to ensure:

- reproducible record positions
- stable boundary definition (e.g., row 100 / 101)
- consistent mapping between logical and physical record layout

This enables controlled failure injection at exact stream locations.

---

#### Fault Injection Techniques

The following techniques may be used independently or in combination:

##### 1. Mid-Stream Truncation
Cutting the stream at a computed byte offset inside a valid record.

##### 2. Boundary Record Injection
Introducing strictly valid records at known positions (e.g., rows 100–101) within a noisier dataset.

##### 3. Mixed-Quality Dataset Generation
Combining baseline noisy records with high-fidelity anchor records.

---

#### Example Test Pattern

- Generate rows 1–99 (baseline dataset)
- Generate row 100 (valid anchor record)
- Generate row 101 (valid anchor or target record)
- Optionally truncate stream at:
  - mid-point of row 101
  - or after row 100 + ½ row 101 size

---

#### Observed Behavior Targets

The system is designed to evaluate:

- whether parsing stops at first corruption or continues
- how partial records are handled
- whether valid segments remain accessible after failure
- how much of the stream is recoverable or usable
- whether behavior is deterministic under repeated runs

---

#### Rationale

Traditional validation focuses on full dataset correctness. This harness focuses instead on:

> **system behavior under partial failure and stream interruption**

This is critical for understanding real-world ingestion systems where data transmission is not atomic.

---

#### Intended Use Cases

- parser robustness testing
- stream ingestion failure modeling
- boundary condition analysis
- ETL resilience validation
- record-based format behavior exploration

---

#### Notes

This project intentionally separates:
- bulk data generation concerns
- boundary correctness evaluation
- failure injection logic

to isolate and observe parser behavior under controlled conditions.

### One-Liner

“It’s like ordering dinner versus cooking yourself—the interface stays simple while execution is delegated to a specialized system.”

"A jigsaw puzzle derives its value primarily from complete reconstruction. Even a small number of missing pieces can significantly reduce its usefulness and may discourage further assembly altogether"

| approach| tag|essentials |
|---------|----|------------------|
|Streaming / Tus-like |cassette tape |continuous, marginal recoverable damage|
|Custom chunked | jigsaw |piecewise, value emerges after assembly|


|Approach	|Tag	|Essentials|
|Streaming / Tus-like|	Cassette tape	|Continuous transfer; partial progress remains meaningful; damage is typically local and partially recoverable|
|Custom chunked	|Jigsaw puzzle|	Piecewise transfer; value emerges after assembly; a missing piece may ruin the usefulness of the whole|


Jigsaw puzzle: Value depends on successful assembly; a missing piece may render the result unusable. For example, when acceptance requires an exact checksum match, the payload is rejected regardless of whether the missing bytes were "important" or seemingly insignificant. A checksum can verify integrity but cannot reconstruct missing information, so an incomplete payload cannot be recovered from the checksum alone.

The acceptance policy belongs to the payload owner, not to the transport mechanism

### See Also:

 
 * [cb2xml](https://github.com/bmTas/cb2xml)
 * [JRecord](https://github.com/bmTas/JRecord)
 * [CobolToJson](https://github.com/bmTas/CobolToJson)
 * [Sourceforge download](https://sourceforge.net/projects/coboltojson/) convert cobol Data Files to JSON
 * [Sourceforge download](https://sourceforge.net/projects/jrecord/files/JRecord/0.93.3/JRecord-0.93.3-src.zip/download) of JRecord jar bundle (old version)
 * [sourceforge project](https://sourceforge.net/projects/jrecord/) 
 * [JRecord Wiki](https://sourceforge.net/p/jrecord/wiki/Home/)
 * [JRecord Discussion](https://sourceforge.net/p/jrecord/discussion/)

---
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
