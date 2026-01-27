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

```cmd
pushd ..\basic-jrecord-dummydata
java -cp target\example.generator.jar;target\lib\* example.Generator  -outputfile example.bin -copybookfile example.cbl -maxrows 1000
copy  /y example.bin ..\basic-jrecord-meteredreader\
popd
```
< NOTE: argument duplication
```cmd
java -cp target\example.metered-reader.jar;target\lib\* example.Reader -debug true -benchmark true -inputfile example.bin -copybookfile example.cbl -maxrows 10 -p copybookFile=example.cbl -p inputFile=example.bin -p maxRows=10
```

```text	

# JMH version: 1.37
# VM version: JDK 11.0.12, OpenJDK 64-Bit Server VM, 11.0.12+7-LTS
# VM invoker: c:\java\jdk-11.0.12\bin\java.exe
# VM options: <none>
# Blackhole mode: full + dont-inline hint (auto-detected, use -Djmh.blackhole.autoDetect=false to disable)
# Warmup: 5 iterations, 10 s each
# Measurement: 5 iterations, 10 s each
# Timeout: 10 min per iteration
# Threads: 1 thread, will synchronize iterations
# Benchmark mode: Throughput, ops/time
# Benchmark: example.CopyBookReaderBenchmark.parseRecordsBencmark
# Parameters: (copybookFile = example.cbl, inputFile = example.bin, maxRows = 10, page = cp037)

# Run progress: 80.00% complete, ETA 00:01:40
# Fork: 5 of 5
# Warmup Iteration   1: 616.287 ops/s
# Warmup Iteration   2: 1072.765 ops/s
# Warmup Iteration   3: 1248.064 ops/s
# Warmup Iteration   4: 1205.179 ops/s
# Warmup Iteration   5: 1296.014 ops/s
Iteration   1: 1273.159 ops/s
Iteration   2: 1306.431 ops/s
Iteration   3: 1228.301 ops/s
Iteration   4: 1136.766 ops/s
Iteration   5: 1206.760 ops/s


Result "example.CopyBookReaderBenchmark.parseRecordsBencmark":
  3173.939 ±(99.9%) 1120.520 ops/s [Average]
  (min, avg, max) = (1136.766, 3173.939, 4653.024), stdev = 1495.862
  CI (99.9%): [2053.420, 4294.459] (assumes normal distribution)


# Run complete. Total time: 00:08:23

REMEMBER: The numbers below are just data. To gain reusable insights, you need to follow up on
why the numbers are the way they are. Use profilers (see -prof, -lprof), design factorial
experiments, perform baseline and negative tests that provide experimental control, make sure
the benchmarking environment is safe on JVM/OS/HW level, ask for reviews from the domain experts.
Do not assume the numbers tell you what you want them to tell.

Benchmark                                     (copybookFile)  (inputFile)  (maxRows)  (page)   Mode  Cnt     Score      Error  Units
CopyBookReaderBenchmark.parseRecordsBencmark     example.cbl  example.bin         10   cp037  thrpt   25  3173.939 ± 1120.520  ops/s

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
