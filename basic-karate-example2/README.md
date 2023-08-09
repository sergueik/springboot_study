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

Alternatiely delete files
```cmd
%userprofile%\test.trace.db
%userprofile%\test.mv.db
```

restart to observe

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

### See Also


### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
