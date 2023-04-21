### Info


Replica of java based timeseries event data stream and metrics storage engine using MongoDB [project](https://github.com/aparnachaudhary/mongodb-metrics-store) 
by Aparna Chaudhary


updatd to make build

### Usage

* run mongodb `Dockerfile.mongodb` from sibling project `basic-mongo`:
```sh
IMAGE=mongodb
CONTAINER=mongo
docker build -t $IMAGE -f Dockerfile.$IMAGE .
docker container prune -f
docker run -d --name $CONTAINER -p 27017:27017 -i $IMAGE
docker logs $CONTAINER
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
        at org.bson.io.Bits.readFully(Bits.java:33)
        at org.bson.io.Bits.readFully(Bits.java:28)
        at com.mongodb.Response.<init>(Response.java:40)
        at com.mongodb.DBPort.go(DBPort.java:142)
        at com.mongodb.DBPort.go(DBPort.java:106)
        at com.mongodb.DBPort.findOne(DBPort.java:162)
        at com.mongodb.DBPort.runCommand(DBPort.java:170)
        at com.mongodb.DBTCPConnector.initDirectConnection(DBTCPConnector.java:547)
        at com.mongodb.DBTCPConnector.checkMaster(DBTCPConnector.java:526)
        at com.mongodb.DBTCPConnector.innerCall(DBTCPConnector.java:236)
        at com.mongodb.DBTCPConnector.call(DBTCPConnector.java:216)
        at com.mongodb.DBApiLayer$MyCollection.__find(DBApiLayer.java:288)
        at com.mongodb.DBApiLayer$MyCollection.__find(DBApiLayer.java:273)
        at com.mongodb.DB.getCollectionNames(DB.java:400)
        at org.springframework.data.mongodb.core.MongoTemplate$13.doInDB(MongoTemplate.java:1408)
        at org.springframework.data.mongodb.core.MongoTemplate$13.doInDB(MongoTemplate.java:1406)
        at org.springframework.data.mongodb.core.MongoTemplate.execute(MongoTemplate.java:391)
        at org.springframework.data.mongodb.core.MongoTemplate.getCollectionNames(MongoTemplate.java:1406)
        at net.arunoday.metric.store.repository.impl.GaugeEventRepositoryImpl.findEventTypes(GaugeEventRepositoryImpl.java:61)
        at net.arunoday.metric.store.service.impl.MetricAggregationServiceImpl.performAggregationPerMinute(MetricAggregationServiceImpl.java:29)
        at sun.reflect.NativeMethodAccessorImpl.invoke0(Native Method)
        at sun.reflect.NativeMethodAccessorImpl.invoke(NativeMethodAccessorImpl.java:62)
        at sun.reflect.DelegatingMethodAccessorImpl.invoke(DelegatingMethodAccessorImpl.java:43)
        at java.lang.reflect.Method.invoke(Method.java:498)
        at org.springframework.scheduling.support.ScheduledMethodRunnable.run(ScheduledMethodRunnable.java:65)
        at org.springframework.scheduling.support.DelegatingErrorHandlingRunnable.run(DelegatingErrorHandlingRunnable.java:54)
        at org.springframework.scheduling.concurrent.ReschedulingRunnable.run(ReschedulingRunnable.java:81)
        at java.util.concurrent.Executors$RunnableAdapter.call(Executors.java:511)
        at java.util.concurrent.FutureTask.run(FutureTask.java:266)
        at java.util.concurrent.ScheduledThreadPoolExecutor$ScheduledFutureTask.access$201(ScheduledThreadPoolExecutor.java:180)
        at java.util.concurrent.ScheduledThreadPoolExecutor$ScheduledFutureTask.run(ScheduledThreadPoolExecutor.java:293)
        at java.util.concurrent.ThreadPoolExecutor.runWorker(ThreadPoolExecutor.java:1149)
        at java.util.concurrent.ThreadPoolExecutor$Worker.run(ThreadPoolExecutor.java:624)
        at java.lang.Thread.run(Thread.java:748)

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
        at com.mongodb.DBTCPConnector.call(DBTCPConnector.java:216)
        at com.mongodb.DBApiLayer$MyCollection.__find(DBApiLayer.java:288)
        at com.mongodb.DBApiLayer$MyCollection.__find(DBApiLayer.java:273)
        at com.mongodb.DB.getCollectionNames(DB.java:400)
        at org.springframework.data.mongodb.core.MongoTemplate$13.doInDB(MongoTemplate.java:1408)
        at org.springframework.data.mongodb.core.MongoTemplate$13.doInDB(MongoTemplate.java:1406)
        at org.springframework.data.mongodb.core.MongoTemplate.execute(MongoTemplate.java:391)
        at org.springframework.data.mongodb.core.MongoTemplate.getCollectionNames(MongoTemplate.java:1406)
        at net.arunoday.metric.store.repository.impl.GaugeEventRepositoryImpl.findEventTypes(GaugeEventRepositoryImpl.java:61)
        at net.arunoday.metric.store.service.impl.MetricAggregationServiceImpl.performAggregationPerMinute(MetricAggregationServiceImpl.java:29)
        at sun.reflect.NativeMethodAccessorImpl.invoke0(Native Method)
        at sun.reflect.NativeMethodAccessorImpl.invoke(NativeMethodAccessorImpl.java:62)
        at sun.reflect.DelegatingMethodAccessorImpl.invoke(DelegatingMethodAccessorImpl.java:43)
        at java.lang.reflect.Method.invoke(Method.java:498)
        at org.springframework.scheduling.support.ScheduledMethodRunnable.run(ScheduledMethodRunnable.java:65)
        at org.springframework.scheduling.support.DelegatingErrorHandlingRunnable.run(DelegatingErrorHandlingRunnable.java:54)
        at org.springframework.scheduling.concurrent.ReschedulingRunnable.run(ReschedulingRunnable.java:81)
        at java.util.concurrent.Executors$RunnableAdapter.call(Executors.java:511)
        at java.util.concurrent.FutureTask.run(FutureTask.java:266)
        at java.util.concurrent.ScheduledThreadPoolExecutor$ScheduledFutureTask.access$201(ScheduledThreadPoolExecutor.java:180)
        at java.util.concurrent.ScheduledThreadPoolExecutor$ScheduledFutureTask.run(ScheduledThreadPoolExecutor.java:293)
        at java.util.concurrent.ThreadPoolExecutor.runWorker(ThreadPoolExecutor.java:1149)
        at java.util.concurrent.ThreadPoolExecutor$Worker.run(ThreadPoolExecutor.java:624)
        at java.lang.Thread.run(Thread.java:748)

```

check that mongo to be running

* post few sample events via curl:
```sh
curl -i -X POST http://192.168.0.92:8080/rest/events/ -H "Content-Type: application/json" -d '{"occuredOn":"2013-12-28T14:19:56Z","eventType":"request","value":11.0,"contextData":{"hostname":"localhost"}}'
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
> show tables
```
```text
 request.event
```sh
> db.request.event.find()
```
```javascript
{ "_id" : ObjectId("6442eb28fa78e73d2ab76256"), "_class" : "net.arunoday.metric.store.model.GaugeEvent", "occuredOn" : ISODate("2013-12-28T14:19:56Z"), "eventType" : "request", "value" : 11, "contextData" : { "hostname" : "localhost" } }
{ "_id" : ObjectId("6442eb63fa78e73d2ab76257"), "_class" : "net.arunoday.metric.store.model.GaugeEvent", "occuredOn" : ISODate("2013-12-28T14:19:56Z"), "eventType" : "request", "value" : 11, "contextData" : { "hostname" : "localhost" } }
{ "_id" : ObjectId("6442eb67fa78e73d2ab76258"), "_class" : "net.arunoday.metric.store.model.GaugeEvent", "occuredOn" : ISODate("2013-12-28T14:19:56Z"), "eventType" : "request", "value" : 11, "contextData" : { "hostname" : "localhost" } }
{ "_id" : ObjectId("6442eb73fa78e73d2ab76259"), "_class" : "net.arunoday.metric.store.model.GaugeEvent", "occuredOn" : ISODate("2013-12-28T14:19:56Z"), "eventType" : "request", "value" : 11, "contextData" : { "hostname" : "localhost" } }
{ "_id" : ObjectId("6442ebc8fa78e73d2ab7625a"), "_class" : "net.arunoday.metric.store.model.GaugeEvent", "occuredOn" : ISODate("2013-12-28T14:19:56Z"), "eventType" : "request", "value" : 11, "contextData" : { "hostname" : "localhost" } }
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

![Mongo DB Data Source Configured and Verifed](https://github.com/sergueik/springboot_study/blob/master/basic-mongodb-timeseries/screenshots/capture-mongodb-datasource-enabled.png)

### TODO

* construct time series queries in Grafana Dashboars

### See Also

  * https://github.com/aparnachaudhary/mongodb-metrics-reporter
  * [node.js project](https://github.com/jank6r/simple-mongo-timeseries)
  * [schema design](https://www.mongodb.com/blog/post/schema-design-for-time-series-data-in-mongodb) for Time Series Data in MongoDB
  * [MongoDB plugin for Grafana](https://github.com/JamesOsgood/mongodb-grafana) - early version, available on github
  * latest [MongoDB Data Source Grafana Plugin](https://github.com/JamesOsgood/mongodb-grafana) documentation
  * [setting Up Grafana MongoDB Integration](https://hevodata.com/learn/grafana-mongodb/) 
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
