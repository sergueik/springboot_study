### Info

Source from [Karate UI Automation](https://www.youtube.com/watch?v=_mDU946eQDQ&list=PLMd2VtYMV0OSv62KjzJ4TFGLDTVtTtQVr)  youtube video

NOTE: there is a typo in the sample source shown on the  __UI Testing with Karate__ [blog](https://blog.knoldus.com/ui-testing-with-karate/)
which also attempts to illustrate a later version which does not find any tests (this is a typical for DSL based test suites which are sensitive to directory structure and naming).

### Usage

test through maven
```sh
mvn clean test
```

this will log the completion in cucumber-like format
```text

1 of 1: classpath:karate/TestRunnerUi.feature
---------------------------------------------------------
feature: classpath:karate/TestRunnerUi.feature
report: target/surefire-reports/karate.TestRunnerUi.json
scenarios:  1 | passed:  1 | failed:  0 | time: 11.7077
---------------------------------------------------------
Karate version: 0.9.5
======================================================
elapsed:  12.46 | threads:    1 | thread time: 11.71 
features:     1 | ignored:    0 | efficiency: 0.94
scenarios:    1 | passed:     1 | failed: 0
======================================================

```
after the run there will be a directory  `target/chrome_${SESSION_ID}/` created in `target` with a big number of files and directories within
```
SESSION_ID=1674596237160
find  target/chrome_$SESSION_ID/  -type f |wc
    775     948   73196
find  target/chrome_$SESSION_ID/  -type d |wc
    507     564   46628

```
the majority of files are "data":
```sh
find target/chrome_1674596237160/ -type f -exec file {} \;
```
### Note

NOTE: the run on a clean 64 bit Windows machine in the absence of `karate-config.js`
```sh
mvn test
```
fails with attempting to launch 32 bit Chrome browser

```text
[INFO] Running karate.TestRunnerUi
22:27:06.416 [main] INFO com.intuit.karate.Runner - waiting for parallel features to complete ...
22:27:06.557 [ForkJoinPool-1-worker-1] WARN com.intuit.karate - skipping bootstrap configuration: could not find or read file: classpath:karate-config.js
22:27:06.619 [ForkJoinPool-1-worker-1] DEBUG com.jayway.jsonpath.internal.path.CompiledPath - Evaluating path: $
22:27:06.619 [ForkJoinPool-1-worker-1] DEBUG com.jayway.jsonpath.internal.path.CompiledPath - Evaluating path: $
22:27:06.651 [ForkJoinPool-1-worker-1] DEBUG com.intuit.karate.shell.Command - found / verified free local port: 9222
22:27:06.651 [chrome_1674617226635] DEBUG com.intuit.karate.driver.chrome_1674617226635 - command: [C:\Program Files (x86)\Google\Chrome\Application\chrome.exe, --remote-debugging-port=9222, --no-first-run, --user-data-dir=C:\developer\sergueik\springboot_study\basic-karate-example\target\chrome_1674617226635, --disable-popup-blocking]
22:27:06.651 [ForkJoinPool-1-worker-1] DEBUG com.intuit.karate.driver.chrome_1674617226635 - poll attempt #0 for port to be ready - localhost:9222
22:27:06.651 [chrome_1674617226635] ERROR com.intuit.karate.shell.Command - command error: [C:\Program Files (x86)\Google\Chrome\Application\chrome.exe, --remote-debugging-port=9222, --no-first-run, --user-data-dir=C:\developer\sergueik\springboot_study\basic-karate-example\target\chrome_1674617226635, --disable-popup-blocking] - Cannot run program "C:\Program Files (x86)\Google\Chrome\Application\chrome.exe" (in directory "target\chrome_1674617226635"): CreateProcess error=2, The system cannot find the file specified
```


indicating that Karate is expecting a 32 bit Chrome application on Windows
for testing, one can download a 32 bit version of Chrome from __ Google Chrome Older Versions Download (Windows, Linux & Mac)__ [page](https://www.slimjet.com/chrome/google-chrome-old-version.php)

Note: immediately after a fresh install, one has to manually get past the __Make Chrome your own__ pages in the session run by Karate,  the test itself is unable to operate



Also Junit4 appears to ignore the Karate cucumber-like failed status and reports overal test as pass:

```text
---------------------------------------------------------
Karate version: 0.9.5
======================================================
elapsed:  22.89 | threads:    1 | thread time: 21.86
features:     1 | ignored:    0 | efficiency: 0.95
scenarios:    1 | passed:     0 | failed: 1
======================================================
failed features:
karate.TestRunnerUi: TestRunnerUi.feature:20 - path: $, actual: 'https://www.youtube.com/', expected: 'https://www.youtube.com/results?search_query=knoldus', reason: not equal

[INFO] Tests run: 1, Failures: 0, Errors: 0, Skipped: 0, Time elapsed: 24.626 s - in karate.TestRunnerUi
22:48:08.123 [chrome_1674618465865] 
WARN com.intuit.karate.shell.Command - exitcode was non-zero: 1 - [C:\Program Files (x86)\Google\Chrome\Application\chrome.exe, --remote-debugging-port=9222, --no-first-run, --user-data-dir=C:\developer\sergueik\springboot_study\basic-karate-example\target\chrome_1674618465865, --disable-popup-blocking]
[INFO]
[INFO] Results:
[INFO]
[INFO] Tests run: 1, Failures: 0, Errors: 0, Skipped: 0
[INFO]
[INFO] ------------------------------------------------------------------------
[INFO] BUILD SUCCESS
[INFO] ------------------------------------------------------------------------
[INFO] Total time:  29.192 s
[INFO] Finished at: 2023-01-24T22:48:10-05:00
[INFO] ------------------------------------------------------------------------
```


### See Also

   * [Karate-UI Test Automation framework](https://www.youtube.com/watch?v=NwWIbNG6oXs&list=PLhNpGuN8mVmGBwsRliG2teFEfQLmCKN5Q) youtube videos - may be not in the correct order

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
