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

This adds a bit of extra lag possibly due to challenge of 64 bit apps when communicating with 32 bit apps on Windows
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
   *  youtube course
      + [01 - Introduction](https://www.youtube.com/watch?v=_mDU946eQDQ&list=PLMd2VtYMV0OSv62KjzJ4TFGLDTVtTtQVr&index=1)
      + [02 - Wildcards & Friendly Locators](https://www.youtube.com/watch?v=sgXpDLWN1x0&list=PLMd2VtYMV0OSv62KjzJ4TFGLDTVtTtQVr&index=2)
      + [03 - Browser and Driver Commands](https://www.youtube.com/watch?v=dQGDi6QhpPI&list=PLMd2VtYMV0OSv62KjzJ4TFGLDTVtTtQVr&index=3)
      + [04 - Howto Run in Multiple Browsers](https://www.youtube.com/watch?v=IMdwDKaV8xI&list=PLMd2VtYMV0OSv62KjzJ4TFGLDTVtTtQVr&index=4)
      + [How to Rerun Failed Tests](https://www.youtube.com/watch?v=__9gWDmYMzs&list=PLMd2VtYMV0OSv62KjzJ4TFGLDTVtTtQVr&index=6)
      + [05 - How to take Screenshots](https://www.youtube.com/watch?v=cKV-IsJFE9w&list=PLMd2VtYMV0OSv62KjzJ4TFGLDTVtTtQVr&index=5)
      + [06 - File Upload](https://www.youtube.com/watch?v=bGRcWN3zcOY&list=PLMd2VtYMV0OSv62KjzJ4TFGLDTVtTtQVr&index=7)
      + [07 - Dynamic Web Elements](https://www.youtube.com/watch?v=AE_4xxNTQk0&list=PLMd2VtYMV0OSv62KjzJ4TFGLDTVtTtQVr&index=8)
      + [08 - script method, Javascript only Shadow Root DOM](https://www.youtube.com/watch?v=O76h9Hf9-Os&list=PLMd2VtYMV0OSv62KjzJ4TFGLDTVtTtQVr&index=9)
      + [09 - Send Keybaord keys and Getting Attribues](https://www.youtube.com/watch?v=pV-DL4Mqgxg&list=PLMd2VtYMV0OSv62KjzJ4TFGLDTVtTtQVr&index=10)

   * [Karate-UI Test Automation framework](https://www.youtube.com/watch?v=NwWIbNG6oXs&list=PLhNpGuN8mVmGBwsRliG2teFEfQLmCKN5Q) youtube videos - may be not in the correct order
   * [documentation](https://www.manual2automation.com/Karate/karateConfig.html) of `Karate-Config.Js`
   * [discussion](https://stackoverflow.com/questions/66828388/get-frequent-chrome-driver-error-driver-config-start-failed-chrome-server-re) about `karate-config.js`
   * [blog](https://dev.to/promode/how-to-work-with-karate-config-js-to-switch-env-in-karate-api-testing-automation-19dn) about `karate-config.js`
   * [donfiguring browser exetable path](https://stackoverflow.com/questions/60580582/how-to-launch-all-karate-features-setting-up-which-browser-to-use-as-an-external)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
