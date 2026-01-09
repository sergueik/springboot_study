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

and the realistic, single REGEX matcher expression that captures the entire teller record into named groups, respecting field widths and masks, in the same spirit as your current RGB
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

---
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
