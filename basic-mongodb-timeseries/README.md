### Info


Replica of java based timeseries event data stream and metrics storage engine using MongoDB [project](https://github.com/aparnachaudhary/mongodb-metrics-store)
by Aparna Chaudhary


updated to be able to build

### Usage

* run mongodb `Dockerfile.mongodb` from sibling project `basic-mongo`:
```sh
IMAGE=mongodb
CONTAINER=mongo
docker build -t $IMAGE -f Dockerfile.$IMAGE .
docker container prune -f
docker run -d --name $CONTAINER -p 27017:27017 -i $IMAGE
docker logs -f $CONTAINER
```
* run `grafana` `Dockerfile` from sibling project `basic-grafana` linking it to
`mongo`:

```sh
IMAGE=basic-grafana
docker build -f Dockerfile -t $IMAGE .
```
followed by
```sh
SERVER=mongo
NAME=grafana
docker stop $NAME
docker container rm  $NAME
docker container run --name $NAME --link $SERVER -d -p 3000:3000 $IMAGE
```
* run the application
```sh
mvn -Dmaven.test.skip=true clean install -pl core jetty:run
mvn clean install -pl core jetty:run
```
if observe application
 failing at runtime with
```text
Apr 21, 2023 3:46:00 PM com.mongodb.DBTCPConnector initDirectConnection
WARNING: Exception executing isMaster command on /127.0.0.1:27017
java.io.EOFException
        at org.bson.io.Bits.readFully(Bits.java:48)
   ...
Apr 21, 2023 3:46:00 PM com.mongodb.DBPortPool gotError
WARNING: emptying DBPortPool to /127.0.0.1:27017 b/c of error
java.net.SocketException: Connection reset
        at java.net.SocketInputStream.read(SocketInputStream.java:210)
        at java.net.SocketInputStream.read(SocketInputStream.java:141)
        at java.io.BufferedInputStream.fill(BufferedInputStream.java:246)
        at java.io.BufferedInputStream.read1(BufferedInputStream.java:286)
        at java.io.BufferedInputStream.read(BufferedInputStream.java:345)
        at org.bson.io.Bits.readFully(Bits.java:46)
        at org.bson.io.Bits.readFully(Bits.java:33)
        at org.bson.io.Bits.readFully(Bits.java:28)
        at com.mongodb.Response.<init>(Response.java:40)
        at com.mongodb.DBPort.go(DBPort.java:142)
        at com.mongodb.DBPort.call(DBPort.java:92)
        at com.mongodb.DBTCPConnector.innerCall(DBTCPConnector.java:244)
...
```

check that mongo to be running

* post few sample events via curl:
```sh
curl -i -X POST http://192.168.0.92:8080/rest/events/ -H "Content-Type: application/json" -d '{"occuredOn":"2013-12-28T14:19:56Z","eventType":"request","value":11.0,"contextData":{"hostname":"localhost"}}'
```
```sh
for N in $(seq 0 1 120) ; do D=$(date +%DT%T%Z); curl -i -X POST http://localhost:8080/rest/events/ -H 'Content-Type: application/json' -d "{\"occuredOn\":\"$D\",\"eventType\":\"request\",\"value\":$N,\"contextData\":{\"hostname\":\"localhost\"}}" ; sleep 1; done 
```
the date format `+%DT%T%Z` is not recognized leading to status `400`

```text 
HTTP/1.1 400 Bad Request
Content-Type: text/html; charset=ISO-8859-1
Cache-Control: must-revalidate,no-cache,no-store
Content-Length: 305
Server: Jetty(9.1.0.v20131115)

<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1"/>
<title>Error 400 Bad Request</title>
</head>
<body><h2>HTTP ERROR 400</h2>
<p>Problem accessing /rest/events/. Reason:
<pre>    Bad Request</pre></p><hr><i><small>Powered by Jetty://</small></i><hr/>

</body>
</html>

```
and exception in jetty logs:

```text
14:19:28.376 [qtp1779615656-13] DEBUG o.s.w.s.m.m.a.ExceptionHandlerExceptionResolver - Resolving exception from handler [public java.lang.String net.arunoday.metric.store.service.impl.GaugeEventRestService.storeEvent(net.arunoday.metric.store.model.GaugeEvent)]: org.springframework.http.converter.HttpMessageNotReadableException: Could not read JSON: Can not construct instance of java.util.Date from String value '04/23/23T14:19:28EDT': not a valid representation (error: Failed to parse Date value '04/23/23T14:19:28EDT': Can not parse date "04/23/23T14:19:28EDT": not compatible with any of standard forms ("yyyy-MM-dd'T'HH:mm:ss.SSSZ", "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'", "EEE, dd MMM yyyy HH:mm:ss zzz", "yyyy-MM-dd"))
 at [Source: HttpInputOverHTTP@2ef48e97; line: 1, column: 2] (through reference chain: net.arunoday.metric.store.model.GaugeEvent["occuredOn"]); nested exception is com.fasterxml.jackson.databind.exc.InvalidFormatException: Can not construct instance of java.util.Date from String value '04/23/23T14:19:28EDT': not a valid representation (error: Failed to parse Date value '04/23/23T14:19:28EDT': Can not parse date "04/23/23T14:19:28EDT": not compatible with any of standard forms ("yyyy-MM-dd'T'HH:mm:ss.SSSZ", "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'", "EEE, dd MMM yyyy HH:mm:ss zzz", "yyyy-MM-dd"))
 at [Source: HttpInputOverHTTP@2ef48e97; line: 1, column: 2] (through reference chain: net.arunoday.metric.store.model.GaugeEvent["occuredOn"])
```
to format event date like `2013-12-28T14:19:56Z`
use the argument
```sh
D=$(date +%Y-%m-%dT%H:%M:%SZ)
```
```sh
for N in $(seq 0 1 120) ; do D=$(date +%Y-%m-%dT%H:%M:%SZ); curl -i -X POST http://localhost:8080/rest/events/ -H 'Content-Type: application/json' -d "{\"occuredOn\":\"$D\",\"eventType\":\"request\",\"value\":$N,\"contextData\":{\"hostname\":\"localhost\"}}" ; sleep 1; done 
```

* connect to mongo and observe data:
```sh
docker exec -it mongo sh
```

```sh
mongo
```
```sh
> show databases
```
```text
admin       0.000GB
config      0.000GB
eventstore  0.000GB
local       0.000GB
```
```sh
> use eventstore
```
```text
switched to db eventstore
```
```sh
> show tables
```
```text
 request.event
```sh
> db.request.event.aggregate()
```
```javascript
{ "_id" : ObjectId("64457800ec58c3ca491ddad9"), "_class" : "net.arunoday.metric.store.model.GaugeEvent", "occuredOn" : ISODate("2023-04-23T14:25:04Z"), "eventType" : "request", "value" : 0, "contextData" : { "hostname" : "localhost" } }
{ "_id" : ObjectId("64457801ec58c3ca491ddada"), "_class" : "net.arunoday.metric.store.model.GaugeEvent", "occuredOn" : ISODate("2023-04-23T14:25:05Z"), "eventType" : "request", "value" : 1, "contextData" : { "hostname" : "localhost" } }
{ "_id" : ObjectId("64457802ec58c3ca491ddadb"), "_class" : "net.arunoday.metric.store.model.GaugeEvent", "occuredOn" : ISODate("2023-04-23T14:25:06Z"), "eventType" : "request", "value" : 2, "contextData" : { "hostname" : "localhost" } }
...
```
to visualize data from MongoDB in Grafana , will need to install [MongoDB Data Surce Grafana plugin](https://grafana.com/grafana/plugins/grafana-mongodb-datasource/) which is not free
The [old build of the plugin](https://github.com/JamesOsgood/mongodb-grafana)
is avaialable on github.
it can be installed into a working `grafana` container and built there after the start:
```sh
docker exec -it grafana sh
```
```sh
cd /var/lib/grafana/plugins/grafana-mongodb-datasource/
apk add npm
npm install
npm run server
```
then using the unsigned data source in Grafana

![Mongo DB Data Source](https://github.com/sergueik/springboot_study/blob/master/basic-mongodb-timeseries/screenshots/capture-mongodb-datasource-unsigned.png)

it can be successfully configured
with 

  * Host URL `http://localhost:3333` 
  * MongoDB URL  `mongodb://mongo:27017` 
  * MongoDB Database `eventstore`

![Mongo DB Data Source Configured and Verifed](https://github.com/sergueik/springboot_study/blob/master/basic-mongodb-timeseries/screenshots/capture-mongodb-datasource-enabled.png)

### TODO

* construct time series queries in Grafana Dashboars

![Mongo DB Dashboard MongoDB Data Source Query Error](https://github.com/sergueik/springboot_study/blob/master/basic-mongodb-timeseries/screenshots/capture-dashboard-mongodb-datasource-error.png)

### See Also

  * https://github.com/aparnachaudhary/mongodb-metrics-reporter
  * [node.js project](https://github.com/jank6r/simple-mongo-timeseries)
  * [schema design](https://www.mongodb.com/blog/post/schema-design-for-time-series-data-in-mongodb) for Time Series Data in MongoDB
  * [MongoDB plugin for Grafana](https://github.com/JamesOsgood/mongodb-grafana) - early version, available on github
  * latest [MongoDB Data Source Grafana Plugin](https://github.com/JamesOsgood/mongodb-grafana) documentation
  * [setting Up Grafana MongoDB Integration](https://hevodata.com/learn/grafana-mongodb/) 
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
