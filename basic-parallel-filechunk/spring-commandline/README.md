### Treoublshooting
runtime errors

```txt
Caused by: net.sf.JRecord.Common.RecordException: net.sf.cb2xml.sablecc.parser.ParserException: [Line Number = 1, Column = 56] expecting: number88, number not88

```
are caused 
before it even attempts to read the binary data  

> NOTE: the is invoking on its own - there is no explicit code in the application. The typical stack trace below:

```text
Caused by: java.lang.RuntimeException: Chunk 0 failed
	at example.ChunkWorker.run(ChunkWorker.java:70) ~[classes/:na]
	at example.Runner.processChunk(Runner.java:91) ~[classes/:na]
	at example.Runner.lambda$run$0(Runner.java:72) ~[classes/:na]
	at java.base/java.util.concurrent.CompletableFuture$AsyncRun.run(CompletableFuture.java:1736) ~[na:na]
	... 3 common frames omitted
```
the application code in question is very  boilerplate:
```java
public class ChunkWorker implements Runnable {
	@Override
	public void run() {

		log.info("Processing chunk {} [{} - {}]", chunk.getId(), chunk.getStartOffset(), chunk.getEndOffset());

		ICobolIOBuilder ioBuilder = JRecordInterface1.COBOL.newIOBuilder(inputFile.toString())
				.setFileOrganization(IFileStructureConstants.IO_FIXED_LENGTH).setFont(font);

```
```java
public class LineProcessor {

    private static final Logger log = LoggerFactory.getLogger(LineProcessor.class);

    public void process(AbstractLine line, int chunkId) {

        Map<String, Object> record = new LinkedHashMap<>();

        LayoutDetail layoutDetail = line.getLayout();
        RecordDetail recordDetail = layoutDetail.getRecord(0);

        for (FieldDetail fieldDetail : recordDetail.getFields()) {
            String fieldName = fieldDetail.getName();
            Object value = line.getFieldValue(fieldName).asString().trim();
            record.put(fieldName, value);
        }


```
the code below is not reached:
```java
    public void process(AbstractLine line, int chunkId) {

        Map<String, Object> record = new LinkedHashMap<>();

        LayoutDetail layoutDetail = line.getLayout();
        RecordDetail recordDetail = layoutDetail.getRecord(0);

        for (FieldDetail fieldDetail : recordDetail.getFields()) {

```
```txt
Caused by: net.sf.JRecord.Common.RecordException: net.sf.cb2xml.sablecc.parser.ParserException: [Line Number = 1, Column = 56] expecting: number88, number not88
at net.sf.JRecord.IO.builders.CblIOBuilderMultiSchemaBase.getExternalRecordImpl(CblIOBuilderMultiSchemaBase.java:184) ~[JRecord-0.93.3.jar:na]
	at net.sf.JRecord.IO.builders.IOBuilderBase.getExternalRecord(IOBuilderBase.java:430) ~[JRecord-0.93.3.jar:na]
	at net.sf.JRecord.IO.builders.IOBuilderBase.getLayout(IOBuilderBase.java:515) ~[JRecord-0.93.3.jar:na]
	at net.sf.JRecord.IO.builders.IOBuilderBase.newReader(IOBuilderBase.java:256) ~[JRecord-0.93.3.jar:na]
	at example.ChunkWorker.run(ChunkWorker.java:62) ~[classes/:na]
	... 6 common frames omitted
Caused by: java.lang.RuntimeException: net.sf.cb2xml.sablecc.parser.ParserException: [Line Number = 1, Column = 56] expecting: number88, number not88
	at net.sf.cb2xml.Cb2Xml3$BldrImp.asCobolItemTree(Cb2Xml3.java:313) ~[cb2xml-1.01.08.jar:na]
	at net.sf.JRecord.External.Def.Cb2Xml.getCopybook(Cb2Xml.java:218) ~[JRecord-0.93.3.jar:na]
	at net.sf.JRecord.External.cb2xml.CobolCopybookReader.getCopybook(CobolCopybookReader.java:12) ~[JRecord-0.93.3.jar:na]
	at net.sf.JRecord.External.base.BaseCobolItemLoader.loadCopyBook(BaseCobolItemLoader.java:151) ~[JRecord-0.93.3.jar:na]
	at net.sf.JRecord.External.base.BaseCobolItemLoader.loadCopyBook(BaseCobolItemLoader.java:139) ~[JRecord-0.93.3.jar:na]
	at net.sf.JRecord.External.CobolCopybookLoader.loadCopyBook(CobolCopybookLoader.java:23) ~[JRecord-0.93.3.jar:na]
	at net.sf.JRecord.IO.builders.CreateExternalFromFile.createExternalRecordImp(CreateExternalFromFile.java:48) ~[JRecord-0.93.3.jar:na]
	at net.sf.JRecord.IO.builders.CreateExternalBase.createExternalRecord(CreateExternalBase.java:104) ~[JRecord-0.93.3.jar:na]
	at net.sf.JRecord.IO.builders.CblIOBuilderMultiSchemaBase.getExternalRecordImpl(CblIOBuilderMultiSchemaBase.java:157) ~[JRecord-0.93.3.jar:na]
	... 10 common frames omitted
Caused by: net.sf.cb2xml.sablecc.parser.ParserException: [Line Number = 1, Column = 56] expecting: number88, number not88
	at net.sf.cb2xml.sablecc.parser.Parser.parse(Parser.java:194) ~[cb2xml-1.01.08.jar:na]
	at net.sf.cb2xml.DoCobolAnalyse.run(DoCobolAnalyse.java:159) ~[cb2xml-1.01.08.jar:na]
	at net.sf.cb2xml.DoCobolAnalyse.doAnalysis(DoCobolAnalyse.java:114) ~[cb2xml-1.01.08.jar:na]
	at net.sf.cb2xml.Cb2Xml3$BldrImp.getCopybook(Cb2Xml3.java:321) ~[cb2xml-1.01.08.jar:na]
	at net.sf.cb2xml.Cb2Xml3$BldrImp.asCobolItemTree(Cb2Xml3.java:307) ~[cb2xml-1.01.08.jar:na]
	... 18 common frames omitted
```
that’s peak __Golden COBOL Speak__: *cryptic*, *technically precise*, and totally *unhelpful* *without tribal knowledge*.


is not a runtime data problem at all — it is a copybook grammar parse failure coming from the cb2xml parser that JRecord uses internally

The failure occurs during initial step:
```java
JRecordInterface1.COBOL.newIOBuilder(inputFile.toString())
    .setFileOrganization(IFileStructureConstants.IO_FIXED_LENGTH)
    .setFont(font);
```

and before:
```java
process(AbstractLine line, int chunkId)
```

is ever reached

What does “number88 / number not88” actually mean?

In __COBOL__, `level 88` fields define condition names:

```txt
01 STATUS-CODE        PIC 9.
   88 STATUS-OK       VALUE 1.
   88 STATUS-ERROR    VALUE 2.
```
The cb2xml parser grammar expects NBF:
```text
LEVEL-NUMBER ::= 01 | 02 | ... | 49 | 66 | 77 | 88
```
When it says:
```
expecting: number88, number not88
```

it literally attempts to inform that:

"At this position in the copybook, I expected either: *a valid numeric level number* (like `01`, `05`, `10`), or
an `88-level` condition entry,
but I found something else that doesn't match __COBOL__ grammar". 
Plain and Easy English
This indicates  a syntax error in the copybook, not in the Java code
internally [cb2xml](https://github.com/bmTas/cb2xml) uses a strict grammar parser [SableCC](https://github.com/SableCC/sablecc). Thus, cb2xml supports mostly classic IBM-style copybooks. It may choke oni multiple syntax elements listed below:


 * `REDEFINES` with complex expressions
 * `OCCURS DEPENDING ON`
 * `SIGN IS LEADING SEPARATE`
 * `USAGE COMP-5`
 * `PIC X(1000)`
 * inline comments with `*>`
 * non-ASCII characters
 * `COPY ... REPLACING`
 * missing periods
 * tab characters
 * Invalid or partial copybook
 * Dummy or “toy” COBOL files

### Examine

```sh
xxd -g 1 -l 32 src/main/resources/example.cbl

```
```text
00000000: 30 31 20 53 41 4d 50 4c 45 2d 52 45 43 4f 52 44  01 SAMPLE-RECORD
00000010: 2e 0a 30 35 20 52 45 43 2d 54 59 50 45 20 50 49  ..05 REC-TYPE PI
```
the error 
```text
net.sf.cb2xml.sablecc.parser.ParserException: [Line Number = 1, Column = 7] expecting: number88, number not88
```
indicates  cbl2xml is choking within the `SAMPLE-RECORD` token probably dislikes the dash.
replacing `src\main\resources\example.cbl` with ultra safe no=hyphen version
```txt
01 SAMPLERECORD.
05 RECTYPE PIC X(1).
05 ACCOUNTNUMBER PIC 9(5).
05 CUSTOMERNAME PIC X(20).
05 BALANCEAMOUNT PIC S9(7)V99 COMP-3.
05 TRANSACTIONCOUNT PIC 9(3) COMP-3.
05 STATUSCODE PIC 9(1).
```
```sh
xxd -g 1 -l 64 src/main/resources/example.cbl
00000000: 30 31 20 53 41 4d 50 4c 45 52 45 43 4f 52 44 2e  01 SAMPLERECORD.
00000010: 0a 30 35 20 52 45 43 54 59 50 45 20 50 49 43 20  .05 RECTYPE PIC
00000020: 58 28 31 29 2e 0a 30 35 20 41 43 43 4f 55 4e 54  X(1)..05 ACCOUNT
00000030: 4e 55 4d 42 45 52 20 50 49 43 20 39 28 35 29 2e  NUMBER PIC 9(5).

```
```text
```
```sh
iconv -f UTF-8 -t cp037 src/main/resources/example.cbl > /tmp/example.cbl
perl -pe 's/\x1A/\x25/g' /tmp/example.cbl > src/main/resources/example.cbl
cp /tmp/example.cbl src/main/resources/example.cbl
```
#### TLDR; 

This error is raised by the cb2xml grammar parser while reading the COBOL copybook, before any binary data is processed. It indicates that the copybook contains syntax that does not conform to the subset of COBOL grammar supported by cb2xml (often involving malformed level numbers, 88-level condition entries, or unsupported clauses).

in Plain and Simple English: *I was reading your copybook and ran into something that does not look like valid COBOL (at least to me), so I gave up*

Vanilla Spring `CommandLineRunner` is just a startup hook doesn’t define an eventing model which would support job/step completion event notify.

One can implement an equivalent pattern oneself with plain Spring Boot in a few ways.

### Observatbility
The Spring Batch’s `JobCompletionNotificationListener` Gives

* Callback when a job finishes
* Stire and enable Access to status (`COMPLETED`/`FAILED`)
* Easy wiring via `@Bean` listener

```java
Spring Batch’s JobCompletionNo@Bean
public JobExecutionListener listener() {
    return new JobCompletionNotificationListener();
}
```

one has to handle the same within
```java
@Override
    public void run(String... args) {
```

`try...catch...finally`

```java
public class TaskCompletedEvent extends ApplicationEvent {
    public TaskCompletedEvent(Object source) { super(source); }
}
```
and

```java
@Component
public class MyRunner implements CommandLineRunner {

    @Autowired private ApplicationEventPublisher publisher;

    @Override
    public void run(String... args) {
        // do work...
        publisher.publishEvent(new TaskCompletedEvent(this));
    }
}
```
and subscribe
```java
@Component
public class TaskListener {

    @EventListener
    public void onComplete(TaskCompletedEvent event) {
        System.out.println("Task completed!");
    }
}
```
few lifecycle events are already defined in vanilla Spring Boot:

| Event                     | When                           |
| ------------------------- | ------------------------------ |
| `ApplicationStartedEvent`	| right after the context starts |
| `ApplicationReadyEvent`   | after runners finish           |
| `ApplicationFailedEvent`  | 	if startup fails             |


and the following code will work
```java
@Component
public class StartupListener {

    @EventListener
    public void onReady(ApplicationReadyEvent e) {
        // works like completion notification
    }

    @EventListener
    public void onFailure(ApplicationFailedEvent e) {
        // handle failure
    }
}
```
one can also wrap Logic In a `Task Service` With `Callbacks`

```java
@Service
public class MyTaskService {

    public void execute(Runnable onSuccess, Consumer<Throwable> onFailure) {
        try {
            // work
            onSuccess.run();
        } catch (Throwable t) {
            onFailure.accept(t);
        }
    }
}
```

