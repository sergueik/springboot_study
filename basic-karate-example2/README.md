### Info

this directory contains subject application and tests from the __API Testing an Application with Karate__ [pluralsight course](https://app.pluralsight.com/library/courses/karate-api-testing-application), amended to lower versions of Karate Framework and Java

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
### See Also

   * [REST API Testing with Karate](https://www.baeldung.com/karate-rest-api-testing)
   * [API Testing with Karate](https://www.rajith.me/2020/04/api-testing-with-karate.html)
   * [free fake API for testing and prototyping](https://jsonplaceholder.typicode.com)
   * [Karate Assertions and matching cheat sheet](https://priyankab85.medium.com/cheat-sheet-for-karate-assertions-and-matching-d248383546e0)


### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
