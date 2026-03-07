### Info

this directory contains subject application and tests from the __API Testing an Application with Karate__ [pluralsight course](https://app.pluralsight.com/library/courses/karate-api-testing-application), amended to lower versions of Karate Framework and Java

### Background

Karate is an open-source test automation framework built on Java and JavaScript, yet designed to remove the entry barriers of either language. It uniquely integrates API testing, UI automation, scientifically accurate performance testing (via Gatling), and service virtualization (mocking) into a single cohesive tool. Inspired by Cucumber, Karate employs a simple Gherkin
‑based syntax (`.feature` files) reminiscent of early BDD DSLs independently found in the Ruby ecosystem (`spec` files).
Karate feature files achieve the same goals that spec-driven frameworks in Ruby, Puppet, or InSpec do: tests are understandable and even writable by domain experts without Java or JavaScript experience, making automation accessible.

In the API testing domain, Karate deliberately abandons Cucumber’s full flexibility in favor of a “simple English,” precise scenario subset of Gherkin.

*longer version*:

In the API testing domain, Karate makes an important sacrifice by dropping Cucumber’s full flexibility in favor of a “simple English” precise formal scenario subset of Gherkin langage, naturally aligned with REST API testing. This eliminates the need for boilerplate Java plumbing code, making tests concise, readable, and maintainable without sacrificing clarity or expressiveness.

For UI automation, Karate extends into Selenium-like capabilities, providing a DSL for waits, captures, Shadow DOM, iframe handling, file uploads/downloads, and visual verifications. This addresses all major modern web UI testing needs, from dynamic content synchronization to complex DOM manipulations, offering a comprehensive, low-code solution for both API and UI testing. 

These features place Karate in the same league as advanced JavaScript-based frameworks like 
* Playwright
* Cypress
* TestCafe
* WebDriverIO

but they also make it a clear productivity winner over classic pure Selenium, by reducing boilerplate, simplifying test maintenance, and providing richer built-in support for modern web UI challenges.


Another standout aspect is Karate’s HTML page tree-view for step results and debugging, providing instant, clear, hierarchical visibility into every test step — a level of clarity and maintainability that most modern frameworks do not offer

To get similar functionality with tools like TestNG, SpecFlow, Cucumber, or Selenium, teams typically add external reporting add‑ons that require setup, configuration, and often build tool/plugin work:
* Allure Report – requires explicit setup
* ExtentReports
Combined with its scientifically accurate performance testing, Karate offers a complete, low-code automation ecosystem that is robust, accessible, and highly recognized by its community.

### Usage

if seeing
```text
[INFO] Attaching agents: []
H2 web server started: http://192.168.99.1:8082
H2 db server started: tcp://192.168.99.1:9092
Table exists: useraccounts. Skipped data initialization
Table exists: events. Skipped data initialization
Table exists: eventsubscriptions. Skipped data initialization

```


the workaround is to coonnect to H2 Database via console web interface `http://192.168.99.1:8082/login.do` and confirm there is no data 

```SQL
select count(1) from useraccounts;
select count(1) from useraccounts;
select count(1) from events;
select count(1) from eventsubscriptions;
```

then drop the tabkes

```SQL
select count(1) from useraccounts;
select count(1) from useraccounts;
select count(1) from events;
select count(1) from eventsubscriptions;

```

if see errorabout constraints
```text
```
repeat the drop run until all clear

Alternatiely delete the database files
```cmd
del /q %userprofile%\test.trace.db
del /q %userprofile%\test.mv.db
```

restart 

```cmd
mvn spring-boot:run
```
to observe


```text
H2 web server started: http://192.168.99.1:8082
H2 db server started: tcp://192.168.99.1:9092
Created table useraccounts...
USERACCOUNTS Data inserted successfully.
Created table events...
EVENTS Data inserted successfully.
Created table eventsubscriptions...
EVENTSUBSCRIPTIONS Data inserted successfully.
```

and 
```SQL
select count(1) from useraccounts;
select count(1) from useraccounts;
select count(1) from events;
select count(1) from eventsubscriptions;

```
will see
```text
COUNT
5
COUNT
5
COUNT
50
COUNT
49
```

The
```sh
curl -s http://$(hostname -i):8080/api/publicevents | jq '.'
```
should return

```JSON
{
  "statusCode": 200,
  "errorMessage": null,
  "data": [
    {
      "id": 1,
      "name": "Music Festival",
      "description": "Join us for a weekend of live music performances by renowned artists.",
      "maxCapacity": 1,
      "date": 1694750400000,
      "organizer": "Harmony Productions",
      "location": "123 Main Street, New York, NY",
      "startTime": "6:00 PM",
      "numberOfHours": 8,
      "currentCapacity": 1,
      "userId": 2
    },
... 
```
alternatively, navigate and execute `GET` `/api/publicevents/{id}` `getEvent` through Swagger UI.

```cmd
mvn clean test
```
```text
---------------------------------------------------------
feature: classpath:example/token/token.feature
scenarios:  1 | passed:  1 | failed:  0 | time: 21.9594
---------------------------------------------------------
```
the token will be in the logs:
```text
target\karate-reports\example.token.token.html
target\karate-reports\example.token.token.karate-json.txt
target\karate-reports\karate-summary-json.txt
```

it can be found through
```cmd
type target\karate-reports\example.token.token.karate-json.txt | c:\tools\jq-win64.exe ".scenarioResults[0].stepResults[6]"
```
```JSON
{
  "result": {
    "nanos": 60556763,
    "millis": 60.556763,
    "status": "passed"
  },
  "step": {
    "line": 12,
    "prefix": "*",
    "index": 5,
    "text": "print \"Access Token: \" + access_token"
  },
  "stepLog": "09:31:07.587 [print] Access Token: eyJhbGciOiJIUzUxMiJ9.eyJ1aWQiOjEsInN1YiI6ImpvaG5zQHBvY2lzb2Z0LmNvbSIsImV4cCI6MTY5MTY5MjI2NywiaWF0IjoxNjkxNjc0MjY3fQ.3fewnoYwpBqFODAkQ97hbrsihXjDdNPMLAtzfNj3LZ68re2_QCV-NvZ6NeqxK21d2Hv9ekPaONsZQW5TTq6w6g \n"
}
```
* TODO imprvove the query
### Threads

at the end of Karate run through maven test it reports utilizing the threads:

```text
classpath:example/token/token.feature
Karate version: 1.3.1
======================================================
elapsed:   7.23 | threads:    5 | thread time: 14.94
features:     5 | skipped:    0 | efficiency: 0.41
scenarios:    8 | passed:     8 | failed: 0
======================================================

```

### NOTE
on Windows / Java 1.8 the Karate runs successfully 26 tests 
```text
[INFO]
[INFO] Results:
[INFO]
[INFO] Tests run: 26, Failures: 0, Errors: 0, Skipped: 0
[INFO]
```
but then throws the error:
```text
[INFO] ------------------------------------------------------------------------
[INFO] BUILD FAILURE
[INFO] ------------------------------------------------------------------------
[INFO] Total time:  01:15 min
[INFO] Finished at: 2023-08-12T22:18:04-04:00
[INFO] ------------------------------------------------------------------------
[ERROR] Failed to execute goal org.apache.maven.plugins:maven-surefire-plugin:2.22.2:test (default-test) on project api-tests: There are test failures.
[ERROR]
[ERROR] Please refer to C:\developer\sergueik\springboot_study\basic-karate-example2\api-tests\target\surefire-reports for the individual test results.
[ERROR] Please refer to dump files (if any exist) [date].dump, [date]-jvmRun[N].dump and [date].dumpstream.
[ERROR] The forked VM terminated without properly saying goodbye. VM crash or System.exit called?
[ERROR] Command was cmd.exe /X /C "c:\java\jdk1.8.0_101\jre\bin\java -Dfile.encoding=UTF-8 -jar C:\Users\Serguei\AppData\Local\Temp\surefire5193139620443941896\surefirebooter2463886534091575716.jar C:\Users\Serguei\AppData\Local\Temp\surefire5193139620443941896 2023-08-12T22-16-55_345-jvmRun1 surefire1552301505766016797tmp surefire_05973237234382054702tmp"
```

on Linux host the Karate successully executes 27 tests:

```text
[INFO] Tests run: 1, Failures: 0, Errors: 0, Skipped: 0, Time elapsed: 6.923 s - in example.ExamplesTest
[INFO]
[INFO] Results:
[INFO]
[INFO] Tests run: 27, Failures: 0, Errors: 0, Skipped: 0
[INFO]
[INFO] ------------------------------------------------------------------------
[INFO] BUILD SUCCESS
[INFO] ------------------------------------------------------------------------
[INFO] Total time:  01:07 min
[INFO] Finished at: 2023-08-13T04:24:18+02
```
### See Also

   * [REST API Testing with Karate](https://www.baeldung.com/karate-rest-api-testing)
   * [API Testing with Karate](https://www.rajith.me/2020/04/api-testing-with-karate.html)
   * [free fake API for testing and prototyping](https://jsonplaceholder.typicode.com)
   * [Karate Assertions and matching cheat sheet](https://priyankab85.medium.com/cheat-sheet-for-karate-assertions-and-matching-d248383546e0)
   * [Introduction to JSON Web Tokens](https://jwt.io/introduction)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
