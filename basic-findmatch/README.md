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
![Mapper](https://github.com/sergueik/springboot_study/blob/master/basic-findmatch/screenshots/capture-mapper.png)

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
PatternSyntaxException caught! 
Input length=213, 
First 50 chars="(?<TTBRANCHID>.{5})(?<TTTELLERID>.{6})(?<TTTERMINA", 
Description="named capturing group is missing trailing '>'", 
Index=105, 
Pattern="^(?<TTBRANCHID>.{5})(?<TTTELLERID>.{6})(?<TTTERMINALID>.{4})(?<TTTRANDATE>\d{8})(?<TTTRANTIME>\d{6})(?<TT_ACCOUNT_NUMBER>\d{12}).replaceAll("_", "")(?<TTTRANCODE>.{4})(?<TTAMOUNT>[+-]?\d{13})(?<TTCURRENCY>[A-Z]{3})$"
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

![Serializer](https://github.com/sergueik/springboot_study/blob/master/basic-findmatch/screenshots/capture-serializer.png)
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

### Going Mainframe Scale

We generate a large (~100-field) mainframe-style record and its matching RegEx pattern without yet constructing the latter from metadata YAML.

Purpose:
- Demonstrate JVM `Pattern.compile()` scalability for fixed-width, simply formatted mainframe-style records.
- Provide a concrete baseline before YAML-driven generation begins.


⚠️ This code is *NOT* intended as a final implementation and will be removed once YAML-driven generation is in place.

#### Generating The Test Code
```sh
$ ./scripts/emit-heavy-record.sh
```
```text
RECORD LENGTH = 940
BR001T12345202401301030151234567890120000000342391USDDEP  ATM WITHDRAWAL                APR123BR001T12345202401301030151234567890120000000342391USDDEP  ATM WITHDRAWAL                APR123BR001T12345202401301030151234567890120000000342391USDDEP  ATM WITHDRAWAL                APR123BR001T12345202401301030151234567890120000000342391USDDEP  ATM WITHDRAWAL                APR123BR001T12345202401301030151234567890120000000342391USDDEP  ATM WITHDRAWAL                APR123BR001T12345202401301030151234567890120000000342391USDDEP  ATM WITHDRAWAL                APR123BR001T12345202401301030151234567890120000000342391USDDEP  ATM WITHDRAWAL                APR123BR001T12345202401301030151234567890120000000342391USDDEP  ATM WITHDRAWAL                APR123BR001T12345202401301030151234567890120000000342391USDDEP  ATM WITHDRAWAL                APR123BR001T12345202401301030151234567890120000000342391USDDEP  ATM WITHDRAWAL                APR123

JAVA-READY REGEX STRING:
public static final String regexString = "^" +
"(?<BRANCHID1>.{5})" +
"(?<TELLERID1>.{6})" +
"(?<TRANDATE1>\\d{8})" +
"(?<TRANTIME1>\\d{6})" +
"(?<ACCOUNTNUMBER1>\\d{12})" +
"(?<AMOUNT1>[+-]?\\d{13})" +
"(?<CURRENCY1>[A-Z]{3})" +
"(?<BRANCH1>.{5})" +
"(?<DESCRIPTION1>.{30})" +
"(?<APPROVALCODE1>.{6})" +
"(?<BRANCHID2>.{5})" +
"(?<TELLERID2>.{6})" +
"(?<TRANDATE2>\\d{8})" +
"(?<TRANTIME2>\\d{6})" +
"(?<ACCOUNTNUMBER2>\\d{12})" +
"(?<AMOUNT2>[+-]?\\d{13})" +
"(?<CURRENCY2>[A-Z]{3})" +
"(?<BRANCH2>.{5})" +
"(?<DESCRIPTION2>.{30})" +
"(?<APPROVALCODE2>.{6})" +
"(?<BRANCHID3>.{5})" +
"(?<TELLERID3>.{6})" +
"(?<TRANDATE3>\\d{8})" +
"(?<TRANTIME3>\\d{6})" +
"(?<ACCOUNTNUMBER3>\\d{12})" +
"(?<AMOUNT3>[+-]?\\d{13})" +
"(?<CURRENCY3>[A-Z]{3})" +
"(?<BRANCH3>.{5})" +
"(?<DESCRIPTION3>.{30})" +
"(?<APPROVALCODE3>.{6})" +
"(?<BRANCHID4>.{5})" +
"(?<TELLERID4>.{6})" +
"(?<TRANDATE4>\\d{8})" +
"(?<TRANTIME4>\\d{6})" +
"(?<ACCOUNTNUMBER4>\\d{12})" +
"(?<AMOUNT4>[+-]?\\d{13})" +
"(?<CURRENCY4>[A-Z]{3})" +
"(?<BRANCH4>.{5})" +
"(?<DESCRIPTION4>.{30})" +
"(?<APPROVALCODE4>.{6})" +
"(?<BRANCHID5>.{5})" +
"(?<TELLERID5>.{6})" +
"(?<TRANDATE5>\\d{8})" +
"(?<TRANTIME5>\\d{6})" +
"(?<ACCOUNTNUMBER5>\\d{12})" +
"(?<AMOUNT5>[+-]?\\d{13})" +
"(?<CURRENCY5>[A-Z]{3})" +
"(?<BRANCH5>.{5})" +
"(?<DESCRIPTION5>.{30})" +
"(?<APPROVALCODE5>.{6})" +
"(?<BRANCHID6>.{5})" +
"(?<TELLERID6>.{6})" +
"(?<TRANDATE6>\\d{8})" +
"(?<TRANTIME6>\\d{6})" +
"(?<ACCOUNTNUMBER6>\\d{12})" +
"(?<AMOUNT6>[+-]?\\d{13})" +
"(?<CURRENCY6>[A-Z]{3})" +
"(?<BRANCH6>.{5})" +
"(?<DESCRIPTION6>.{30})" +
"(?<APPROVALCODE6>.{6})" +
"(?<BRANCHID7>.{5})" +
"(?<TELLERID7>.{6})" +
"(?<TRANDATE7>\\d{8})" +
"(?<TRANTIME7>\\d{6})" +
"(?<ACCOUNTNUMBER7>\\d{12})" +
"(?<AMOUNT7>[+-]?\\d{13})" +
"(?<CURRENCY7>[A-Z]{3})" +
"(?<BRANCH7>.{5})" +
"(?<DESCRIPTION7>.{30})" +
"(?<APPROVALCODE7>.{6})" +
"(?<BRANCHID8>.{5})" +
"(?<TELLERID8>.{6})" +
"(?<TRANDATE8>\\d{8})" +
"(?<TRANTIME8>\\d{6})" +
"(?<ACCOUNTNUMBER8>\\d{12})" +
"(?<AMOUNT8>[+-]?\\d{13})" +
"(?<CURRENCY8>[A-Z]{3})" +
"(?<BRANCH8>.{5})" +
"(?<DESCRIPTION8>.{30})" +
"(?<APPROVALCODE8>.{6})" +
"(?<BRANCHID9>.{5})" +
"(?<TELLERID9>.{6})" +
"(?<TRANDATE9>\\d{8})" +
"(?<TRANTIME9>\\d{6})" +
"(?<ACCOUNTNUMBER9>\\d{12})" +
"(?<AMOUNT9>[+-]?\\d{13})" +
"(?<CURRENCY9>[A-Z]{3})" +
"(?<BRANCH9>.{5})" +
"(?<DESCRIPTION9>.{30})" +
"(?<APPROVALCODE9>.{6})" +
"(?<BRANCHID10>.{5})" +
"(?<TELLERID10>.{6})" +
"(?<TRANDATE10>\\d{8})" +
"(?<TRANTIME10>\\d{6})" +
"(?<ACCOUNTNUMBER10>\\d{12})" +
"(?<AMOUNT10>[+-]?\\d{13})" +
"(?<CURRENCY10>[A-Z]{3})" +
"(?<BRANCH10>.{5})" +
"(?<DESCRIPTION10>.{30})" +
"(?<APPROVALCODE10>.{6})" +
"$";

```
#### Running the Test
```sh
mvn test 2>a.json 
cat a.json | jq '.'
```
```json
{
  "APPROVALCODE4": "APR123",
  "TRANDATE9": "20240130",
  "APPROVALCODE5": "APR123",
  "ACCOUNTNUMBER1": "123456789012",
  "APPROVALCODE6": "APR123",
  "CURRENCY9": "USD",
  "APPROVALCODE7": "APR123",
  "ACCOUNTNUMBER3": "123456789012",
  "APPROVALCODE1": "APR123",
  "ACCOUNTNUMBER2": "123456789012",
  "APPROVALCODE2": "APR123",
  "ACCOUNTNUMBER5": "123456789012",
  "APPROVALCODE3": "APR123",
  "ACCOUNTNUMBER4": "123456789012",
  "CURRENCY3": "USD",
  "CURRENCY4": "USD",
  "CURRENCY1": "USD",
  "CURRENCY10": "USD",
  "CURRENCY2": "USD",
  "CURRENCY7": "USD",
  "APPROVALCODE8": "APR123",
  "CURRENCY8": "USD",
  "APPROVALCODE9": "APR123",
  "CURRENCY5": "USD",
  "CURRENCY6": "USD",
  "TRANDATE10": "20240130",
  "TRANTIME9": "103015",
  "TRANTIME8": "103015",
  "ACCOUNTNUMBER7": "123456789012",
  "ACCOUNTNUMBER6": "123456789012",
  "ACCOUNTNUMBER9": "123456789012",
  "ACCOUNTNUMBER8": "123456789012",
  "DESCRIPTION5": "ATM WITHDRAWAL                ",
  "DESCRIPTION4": "ATM WITHDRAWAL                ",
  "TRANTIME10": "103015",
  "DESCRIPTION7": "ATM WITHDRAWAL                ",
  "DESCRIPTION6": "ATM WITHDRAWAL                ",
  "DESCRIPTION9": "ATM WITHDRAWAL                ",
  "DESCRIPTION8": "ATM WITHDRAWAL                ",
  "DESCRIPTION1": "ATM WITHDRAWAL                ",
  "DESCRIPTION3": "ATM WITHDRAWAL                ",
  "DESCRIPTION2": "ATM WITHDRAWAL                ",
  "TRANTIME5": "103015",
  "TRANTIME4": "103015",
  "DESCRIPTION10": "ATM WITHDRAWAL                ",
  "TRANTIME7": "103015",
  "TRANTIME6": "103015",
  "TRANTIME1": "103015",
  "TRANTIME3": "103015",
  "TRANTIME2": "103015",
  "BRANCH10": "BR001",
  "AMOUNT10": "0000000342391",
  "AMOUNT3": "0000000342391",
  "AMOUNT2": "0000000342391",
  "AMOUNT1": "0000000342391",
  "TELLERID9": "T12345",
  "AMOUNT7": "0000000342391",
  "AMOUNT6": "0000000342391",
  "AMOUNT5": "0000000342391",
  "BRANCHID10": "BR001",
  "AMOUNT4": "0000000342391",
  "BRANCHID3": "BR001",
  "BRANCHID4": "BR001",
  "BRANCHID1": "BR001",
  "BRANCHID2": "BR001",
  "TELLERID3": "T12345",
  "TELLERID4": "T12345",
  "TELLERID1": "T12345",
  "BRANCHID9": "BR001",
  "AMOUNT9": "0000000342391",
  "TELLERID10": "T12345",
  "TELLERID2": "T12345",
  "AMOUNT8": "0000000342391",
  "BRANCHID7": "BR001",
  "TELLERID7": "T12345",
  "BRANCHID8": "BR001",
  "TELLERID8": "T12345",
  "BRANCHID5": "BR001",
  "TELLERID5": "T12345",
  "BRANCHID6": "BR001",
  "TELLERID6": "T12345",
  "APPROVALCODE10": "APR123",
  "BRANCH1": "BR001",
  "BRANCH3": "BR001",
  "BRANCH2": "BR001",
  "BRANCH5": "BR001",
  "BRANCH4": "BR001",
  "BRANCH7": "BR001",
  "BRANCH6": "BR001",
  "BRANCH9": "BR001",
  "BRANCH8": "BR001",
  "ACCOUNTNUMBER10": "123456789012",
  "TRANDATE1": "20240130",
  "TRANDATE2": "20240130",
  "TRANDATE3": "20240130",
  "TRANDATE4": "20240130",
  "TRANDATE5": "20240130",
  "TRANDATE6": "20240130",
  "TRANDATE7": "20240130",
  "TRANDATE8": "20240130"
}


```
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

### Note

COBOL copybooks and derivative artifacts (including Excel spreadsheets) are treated like controlled source: their structure defines business logic. Distribution is restricted because even a single file can expose sensitive operational rules, so ‘need-to-know’ proof, approval workflows, artifact signing, and ACLs are universally enforced. Everyone in finance/legacy IT knows this is non-negotiable.

### See Also

 * https://stackoverflow.com/questions/415580/regex-named-groups-in-java
 * [sixface/YamlConfig](https://github.com/jsixface/YamlConfig) and [extension](https://github.com/sergueik/selenium_java/tree/master/yaml_config_extend)

---
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
