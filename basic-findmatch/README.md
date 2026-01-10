###  Info

designed the idea once, then expressed it naturally in each ecosystem:

__C#__ → String extension method returning a single value (idiomatic for __.NET__ at the time)

__Java__ → utility + tests returning a `Map<String, String>` (idiomatic, test-first, more general)

That’s exactly how how experienced engineers work when moving ideas across stacks

### Background

This is a standwlone Project dreiver from Java test class [`FindMatchTest.java`](https://github.com/sergueik/selenium_tests/blob/master/src/test/java/com/github/sergueik/selenium/FindMatchTest.java)

Which in turn was a
Ported/Exteding
from c# [`Matcher.cs`](https://github.com/sergueik/powershell_selenium/blob/master/selenium4/csharp/basic/Extensions/Matcher.cs)
### Testing

Many “copybook-grade” datasets can be flattened into a `Map<String,Object>`, especially for __teller__ / core-banking style records where:

* no `OCCURS DEPENDING ON`,
* no `REDEFINES` (or ignored initially),
* fixed PICs,
* EBCDIC/ASCII conversion handled earlier or mocked.

#### Realistic COBOL copybook (teller / transaction record)

This is not toy __COBOL__ — it’s representative of what __Encore__ /__Hogan__ / __FIS__ / __Unisys__ shops use.
```text
       01  TELLER-TRANSACTION-REC.
           05  TT-HEADER.
               10  TT-BRANCH-ID          PIC X(05).
               10  TT-TELLER-ID          PIC X(06).
               10  TT-TERMINAL-ID        PIC X(04).
               10  TT-TRAN-DATE          PIC 9(08).  *> YYYYMMDD
               10  TT-TRAN-TIME          PIC 9(06).  *> HHMMSS

           05  TT-BODY.
               10  TT-ACCOUNT-NUMBER     PIC 9(12).
               10  TT-TRAN-CODE          PIC X(04).
               10  TT-AMOUNT             PIC S9(11)V99 COMP-3.
               10  TT-CURRENCY           PIC X(03).
               10  TT-DESCRIPTION        PIC X(30).

           05  TT-STATUS.
               10  TT-RESPONSE-CODE      PIC X(02).
               10  TT-APPROVAL-CODE      PIC X(06).

```
the same, as a flat record after __EBCDIC__/__ASCII__ and __COMP-3__ decoding:

```text
BR001T123450012202401301030151234567890123DEP 00000012345.67USDATM WITHDRAWAL            00APR123
```
it should ipiece-meal load into record like below:
```java

String record =
  "BR001" +        // TT-BRANCH-ID
  "T12345" +       // TT-TELLER-ID
  "0012" +         // TT-TERMINAL-ID
  "20240130" +     // TT-TRAN-DATE
  "103015" +       // TT-TRAN-TIME
  "123456789012" + // TT-ACCOUNT-NUMBER
  "DEP " +         // TT-TRAN-CODE
  "0000001234567" +// TT-AMOUNT (scaled later)
  "USD" +          // TT-CURRENCY
  padRight("ATM WITHDRAWAL", 30) +
  "00" +           // TT-RESPONSE-CODE
  "APR123";        // TT-APPROVAL-CODE

```

Mapping file (mirroring what Encore-grade apps actually use)

```YAML

record: TELLER_TRANSACTION
encoding: ASCII
scale:
  TT-AMOUNT: 2

fields:
  - name: TT-BRANCH-ID
    offset: 0
    length: 5
    type: string

  - name: TT-TELLER-ID
    offset: 5
    length: 6
    type: string

  - name: TT-TERMINAL-ID
    offset: 11
    length: 4
    type: string

  - name: TT-TRAN-DATE
    offset: 15
    length: 8
    type: date
    format: yyyyMMdd

  - name: TT-TRAN-TIME
    offset: 23
    length: 6
    type: time
    format: HHmmss

  - name: TT-ACCOUNT-NUMBER
    offset: 29
    length: 12
    type: long

  - name: TT-TRAN-CODE
    offset: 41
    length: 4
    type: string

  - name: TT-AMOUNT
    offset: 45
    length: 13
    type: decimal
    signed: true
    scale: 2

  - name: TT-CURRENCY
    offset: 58
    length: 3
    type: string

  - name: TT-DESCRIPTION
    offset: 61
    length: 30
    type: string
    trim: true

  - name: TT-RESPONSE-CODE
    offset: 91
    length: 2
    type: string

  - name: TT-APPROVAL-CODE
    offset: 93
    length: 6
    type: string

```
![Docker Cluster](https://github.com/sergueik/springboot_study/blob/master/basic-findmatch/screenshots/capture-illustration.png)

resulting Map (in Java 11 syntax):
```java

Map<String, Object> recordMap = Map.of(
    "TT-BRANCH-ID", "BR001",
    "TT-TELLER-ID", "T12345",
    "TT-TERMINAL-ID", "0012",
    "TT-TRAN-DATE", LocalDate.of(2024,1,30),
    "TT-TRAN-TIME", LocalTime.of(10,30,15),
    "TT-ACCOUNT-NUMBER", 123456789012L,
    "TT-TRAN-CODE", "DEP",
    "TT-AMOUNT", new BigDecimal("12345.67"),
    "TT-CURRENCY", "USD",
    "TT-DESCRIPTION", "ATM WITHDRAWAL",
    "TT-RESPONSE-CODE", "00",
    "TT-APPROVAL-CODE", "APR123"
);

```
or in __Java 8__ syntax
```java
Map<String, Object> recordMap = new LinkedHashMap<>();

recordMap.put("TT-BRANCH-ID", "BR001");
recordMap.put("TT-TELLER-ID", "T12345");
recordMap.put("TT-TERMINAL-ID", "0012");
recordMap.put("TT-TRAN-DATE", "20240130");
recordMap.put("TT-TRAN-TIME", "103015");
recordMap.put("TT-ACCOUNT-NUMBER", "123456789012");
recordMap.put("TT-TRAN-CODE", "DEP");
recordMap.put("TT-AMOUNT", "0000001234567");
recordMap.put("TT-CURRENCY", "USD");
recordMap.put("TT-DESCRIPTION", "ATM WITHDRAWAL");
recordMap.put("TT-RESPONSE-CODE", "00");
recordMap.put("TT-APPROVAL-CODE", "APR123");

```

and the realistic, single REGEX matcher expression that captures the entire teller record into named groups, respecting field widths and masks:
```java
private static final String TELLER_RECORD_REGEX =
        "^" +
        "(?<TT_BRANCH_ID>.{5})" +           // PIC X(05)
        "(?<TT_TELLER_ID>.{6})" +           // PIC X(06)
        "(?<TT_TERMINAL_ID>.{4})" +         // PIC X(04)
        "(?<TT_TRAN_DATE>\\d{8})" +         // PIC 9(08)
        "(?<TT_TRAN_TIME>\\d{6})" +         // PIC 9(06)
        "(?<TT_ACCOUNT_NUMBER>\\d{12})" +   // PIC 9(12)
        "(?<TT_TRAN_CODE>.{4})" +            // PIC X(04)
        "(?<TT_AMOUNT>[+-]?\\d{13})" +      // PIC S9(11)V99 (unscaled)
        "(?<TT_CURRENCY>[A-Z]{3})" +         // PIC X(03)
        "(?<TT_DESCRIPTION>.{30})" +         // PIC X(30)
        "(?<TT_RESPONSE_CODE>.{2})" +        // PIC X(02)
        "(?<TT_APPROVAL_CODE>.{6})" +        // PIC X(06)
        "$";

```
### Simplified CopyBook for Testing

> Due to Java regex limitations for very long named-capturing patterns, we simplify the original teller record while keeping realistic, business-meaningful fields.

**Fields included:**

|  Field   | Length | Notes |
|----------|--------|-------|
| BRANCH   | 5      | Branch code |
| TRANDATE | 8      | Transaction date YYYYMMDD |
| ACCOUNT  | 12     | Account number |
| CODE     | 3      | Transaction code |
| AMOUNT   | 9..12  | Amount, unscaled |
| CURRENCY | 3      | Currency code |

it will get:
```text
matching "BR001202401301234567890DEP000012345USD"
regex: ^(?<BRANCH>.{5})(?<TRANDATE>\d{8})(?<ACCOUNT>\d{10,12})(?<CODE>.{3})(?<AMOUNT>\d{9,12})(?<CURRENCY>[A-Z]{3})$
Matched record:
  BRANCH   = BR001
  TRANDATE = 20240130
  ACCOUNT  = 1234567890
  CODE     = DEP
  AMOUNT   = 000012345
  CURRENCY = USD
```
with plain `Regex`
and 
```text
findMatch data:BR003202306151234000001DEP000000789USD
BRANCH: BR003
CURRENCY: USD
ACCOUNT: 1234000001
CODE: DEP
AMOUNT: 000000789
TRANDATE: 20230615

```
with custom class
---

#### Sample simplified record

```java
String record =
  "BR001" +        // BRANCH
  "20240130" +     // TRANDATE
  "123456789012" + // ACCOUNT
  "DEP" +          // CODE
  "0000123456" +   // AMOUNT
  "USD";           // CURRENCY

```
The `FindMatch` class still will be able to construct `result` map keys dynamically from the named groups, it is simply a thin wrapper over library `RegEx` classes.

> Note: in simple English, Feeding Java regex too much "stuff" can make it choke. The engine can slow down dramatically or even crash if patterns are extremely large or complex. Keep regexes simple and split them up when possible — it’s a proven, practical limitation, not just theory.

Examaple crashing Regex package:
```
	@DisplayName("Verify building of the regex with error reporting")
	@ParameterizedTest
	@MethodSource("testDataStream")
	void test(final String data) {
		System.err.println(String.format("testing length=%d", data.length()));
		try {
			Pattern.compile("^" + data + "$");
		} catch (PatternSyntaxException e) {
			// Report more information
        }
```
and output
```text
testing length=213
PatternSyntaxException caught! Input length=213, First 50 chars="(?<TTBRANCHID>.{5})(?<TTTELLERID>.{6})(?<TTTERMINA", Description="named capturing group is missing trailing '>'", Index=105, Pattern="^(?<TTBRANCHID>.{5})(?<TTTELLERID>.{6})(?<TTTERMINALID>.{4})(?<TTTRANDATE>\d{8})(?<TTTRANTIME>\d{6})(?<TT_ACCOUNT_NUMBER>\d{12}).replaceAll("_", "")(?<TTTRANCODE>.{4})(?<TTAMOUNT>[+-]?\d{13})(?<TTCURRENCY>[A-Z]{3})$"
```
### Work In Progress


YAML → Regex utility (`TellerRegexBuilder`)

Responsibility: translate metadata into a single contiguous regex string.
Typically copybook-style YAMLs define fields as lists of maps with offsets/lengths/types:
```yaml
record: TELLER_TRANSACTION
fields:
  - name: TT-BRANCH-ID
    length: 5
  - name: TT-ACCOUNT
    length: 12
  - name: TT-TRAN-CODE
    length: 3
```
this allows for the construction of pattern string 

```java
List<Map<String,Object>> fields = (List<Map<String,Object>>) yamlAccessor.getNode("record.fields", yamlRoot);
for (Map<String,Object> field : fields) {
    String name = (String) field.get("name");
    int length = (Integer) field.get("length");
    regexBuilder.append(String.format("(?<%s>.{%d})", name, length));
}
```
or explicitly
```java
Map<String,Object> field = yamlAccessor.getMap("record.fields[0]");
String name = (String) field.get("name");
int length = (int) field.get("length");

String regexPiece = String.format("(?<%s>.{%d})", name, length);
```
> NOTE: No chunking, no splitting, no “too many groups” logic done by YAML Regex.

Simple, predictable output: one regex per record type.

`FindMatch`

Responsibility: handle field mapping, dealing with runtime limitations (hundreds of groups, __NFA__ load, chunking if needed).

```text

Chunk 1:  Fields 1–25  →  (?<FIELD1>...) ... (?<FIELD25>...) <any string><end of string>


Chunk 2:  Fields 26–50  →  <end of match 1> (?<FIELD26>...) ... (?<FIELD50>...) <any string><end of string>

Chunk 3:  Fields 51–75  → <end of match 2> (?<FIELD51>...) ... (?<FIELD75>...) <any string><end of string>

Chunk 4:  Fields 76–100  → <end of match 3> (?<FIELD76>...) ... (?<FIELD100>...)<end of string>

```
Value-add: Can decide to probe-only, or split into sub-regexes when capturing too many groups.

#### Recursive Chunked Capturing Strategy with head advancement

This approach lets us capture extremely wide records safely, avoiding regex engine overload while keeping the parsing logic linear and predictable

> Note: The phrasing is both punchy and accurate . It communicates the idea quickly to an executive- or engineer-level audience without diving into full technical weeds, while still hinting at the cleverness behind the approach: “recursive” signals repeated application, “chunked” signals manageable pieces, and “head advancement” conveys the subtlety of moving the starting point forward.

#### Serialization

CICS Transaction Copybook Serialization: Deterministically filter out CICS-only fields, inject synthetic metadata (owner UUID, audit, replay, tracing) at emission time, preserve pristine immutable copybook domain data, operate directly on flat maps without requiring explicit POJO classes, enable projection-based transport for API-first services, support schema evolution, and provide human- and machine-readable visualization of field-to-byte associations for compliance, review, and documentation purposes.

sample code:

```java
public class CopyBookSerializer implements JsonSerializer<Map<String,Object>> {

    @Override
    public void serialize(Map<String,Object> value, JsonGenerator jsonGeneraor, SerializerProvider unusedSerializerProvider )
            throws IOException {

        jsonGenerator.writeStartObject();

        // Example: inject metadata
        jsonGenerator.writeStringField("owner_uuid", UUID.randomUUID().toString());

        // Example: filter out unwanted fields
        for (Map.Entry<String,Object> e : value.entrySet()) {
            if (!Set.of("CICS_ONLY_FIELD1","CICS_ONLY_FIELD2").contains(e.getKey())) {
                jsonGenerator.writeObjectField(e.getKey(), e.getValue());
            }
        }

        jsonGenerator.writeEndObject();
    }
}

```
> Note: using an explicit, type-safe, and readable `Map<String,Object>`, not a `T` or raw type `Map` - `JsonSerializer<Map<String,Object>>` is considered best practice.

###  🧾 COBOL Copybook Parsers — Free & Commercial Tools

This overview lists **available copybook parsing tools**, both open source and commercial, that can be used to interpret COBOL copybooks into structured metadata for processing in Java and other languages.

---

### 📦 Free / Open-Source COBOL Copybook Parsers

#### 🟢 **LegStar / Takada COBOL Copybook Parser**
- **Language:** Java  
- **What it does:** Parses COBOL copybooks into Java metadata structures.  
- **Features:**
  - Provides field names, lengths, and types
  - Supports group structures
  - Used in middleware and mainframe integration tools  
- **GitHub:** https://github.com/legstm

#### 🟢 **JRecord**
- **Language:** Java  
- **What it does:** Parses COBOL copybooks and produces record layouts and line value accessors.  
- **Features:**
  - Handles `OCCURS`
  - Supports `REDEFINES`
  - Deals with numeric comp-3 (packed decimals), signed types
  - Works with EBCDIC and ASCII
- **GitHub:** https://github.com/BigLou/jrecord

#### 🟢 **RecordEditor**
- **Built on:** JRecord  
- **What it does:** Desktop application for viewing/editing records defined by copybooks.  
- **Features:**
  - Can export record definitions (e.g., to XML)
  - Useful for inspection and manual mapping creation

#### 🟢 **OpenCobolParser**
- **Language:** Java  
- **What it does:** Full COBOL grammar parser that can parse COBOL source, including copybooks.  
- **Features:**
  - Produces an abstract syntax tree (AST)
  - More complete language coverage than simple copybook text parsers  
- **GitHub:** https://github.com/open-cobol/parser

#### 🟢 **Cobol85Parser (ANTLR Grammar)**
- **Language:** ANTLR grammar usable in Java  
- **What it does:** Grammar for COBOL 85 that can be used with ANTLR to build parsers.  
- **Features:**
  - Generates parser/lexer in Java
  - Needs additional logic to pull out meaningful metadata

---

### 🏢 Commercial COBOL Copybook Tools

#### 🔵 **Micro Focus Enterprise Analyzer**
- **Type:** Commercial enterprise tool  
- **What it does:** Analyzes and parses COBOL applications at scale.  
- **Features:**
  - Builds cross-reference databases
  - Provides structural metadata about programs and copybooks

#### 🔵 **Heirloom Computing / COBOL Conversion Platforms**
- **Type:** Commercial code transformation suite  
- **What it does:** Translates COBOL to Java, C#, or other languages.  
- **Features:**
  - Copybook understanding embedded as part of conversion
  - Generates intermediate schemas and metadata

#### 🔵 **GnuCOBOL Copybook Utility**
- **Type:** Free toolchain component with commercial usage support  
- **What it does:** Converts COBOL copybooks to C headers.  
- **Features:**
  - Generates definitions that can be consumed by other tools

---

### 🧾 Summary Table

| Category     | Tool                       | Language   | Copybook Focus                   |
|-------------|----------------------------|------------|----------------------------------|
| Open Source | **LegStar**                | Java       | Metadata extraction              |
| Open Source | **JRecord**                | Java       | Full layout, OCCURS, signed      |
| Open Source | **RecordEditor**           | Java (GUI) | Visualization/export             |
| Open Source | **OpenCobolParser**        | Java       | Full COBOL grammar               |
| Open Source | **Cobol85Parser (ANTLR)**  | Java       | Grammar for custom parsing       |
| Commercial  | **Micro Focus Enterprise Analyzer** | Enterprise | Full application parsing      |
| Commercial  | **Heirloom Computing**     | Code migration | Copybook embedded             |
| Commercial/Free | **GnuCOBOL**           | C          | Copybook → C headers             |

---

**Notes:**
- For **Java-centric workflows**, LegStar and JRecord are the most common starting points.  
- **JRecord** has wide adoption for ETL and fixed-width file processing environments.  
- Open source parsers vary in completeness; choose based on your copybook features (e.g., OCCURS, REDEFINES).  
- Commercial tools are generally used in **mainframe modernization projects**.


### See Also

 * https://stackoverflow.com/questions/415580/regex-named-groups-in-java
 * [sixface/YamlConfig](https://github.com/jsixface/YamlConfig) and [extension](https://github.com/sergueik/selenium_java/tree/master/yaml_config_extend)

---
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
