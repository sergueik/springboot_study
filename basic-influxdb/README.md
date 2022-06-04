### Info

this directory contains a minimally modified `InfluxDB::Client::Simple` Perl [module](https://metacpan.org/dist/InfluxDB-Client-Simple/source/lib/InfluxDB/Client/Simple.pm)

tested interacting with an InfluxDB __1.8__ [hosted on alpine](https://hub.docker.com/r/woahbase/alpine-influxdb/). Note, this build does not have web interface:

```sh
http://192.168.0.29:8086/
```

is
```text
404 page not found
```

### Testing
#### Run InfluxDB Server
* pull the image
```sh
docker pull woahbase/alpine-influxdb:x86_64
```
* run in the default configuration
```sh
docker run -d -p 8086:8086 woahbase/alpine-influxdb:x86_64
```
```sh
IMAGE=$(docker ps | grep 'woahbase/alpine-influxdb'| awk '{print $1}')
```
#### Create Schema
connect to shell in the container
docker exec -it $IMAGE sh
```
in the container use `influx` to create database:

```sh
/ # influx
```
```text
Connected to http://localhost:8086 version 1.8.3
InfluxDB shell version: 1.8.3
```

```SQL
> CREATE DATABASE example
>
> SHOW DATABASES
```
```text
name: databases
name
----
_internal
example
>
```
#### Test Client
disconnect from container, run Perl script from the host to post data
```sh
perl -I . test.pl
```
repeat a few times.

Note: currently there is a number of dependencie we intend to get eventually rid of, since we are likely not going to need UDP and will be able to use as pureperl module on `alpine-perl` image containers

run shell in the container again
```sh
docker exec -it $IMAGE sh
```
```sh
influx
```
switch to the database created earlier
```SQL
> show databases
```
```text
name: databases
name
----
_internal
example
```
```SQL
> use example
```
```text
Using database example
```
```SQL
> SELECT host,statement from testing
```
```text
name: testing
time                host        statement
----                ----        ---------
1654295907173950198 containment 42
1654296416171853241 containment 42
```

alternatively pull vendor __1.x__ image
```sh
docker pull influxdb:1.7-alpine
```
```sh
docker run -d -p 8086:8086 influxdb:1.7-alpine
```
update environment to connect to shell in the container
```sh
IMAGE=$(docker ps | grep 'influxdb:1.7-alpine'| awk '{print $1}')
```
### Cleanup
```sh
docker container stop $IMAGE
docker container rm $IMAGE
```

### Testing InfluxDB 2.x

pull a vendor image
```sh
docker pull influxdb:2.2.0-alpine
```

```sh
docker run -d -p 8086:8086 influxdb:2.2.0-alpine
```


With __2.x__ need to start in web interface `http://192.168.0.29:8086/`:

![setup Page](https://github.com/sergueik/springboot_study/blob/master/basic-influxdb/screenshots/capture-initial-setup.png)
```
in the container use `influx` to create database:

```sh
/ # influx
```
```text
Connected to http://localhost:8086 version 1.8.3
InfluxDB shell version: 1.8.3
```

```SQL
> CREATE DATABASE example
>
> SHOW DATABASES
```
```text
name: databases
name
----
_internal
example
>
```
disconnect from container, run Perl script from the host
to post data
```sh
perl -I . test.pl
```


NOTE: the default vendor image is based
```sh
docker pull influxdb:2.2.0
```
this makes the image a little heavy - over 300 MB:
```sh
docker image ls | grep influx
```
```text
influxdb                         2.2.0                   5b24dbcedefc   5 days ago       341MB
```
```sh
docker run -d -p 8086:8086 influxdb:2.2.0
```
also it appears to require authentication

### Java Client
```SQL
delete from testing
```
```cmd
mvn package
java -cp target\example.influxdb-client.jar;target\lib\* example.App
```
```text
1.7.11
Starting
.
.
.
.
.
.
.
.
.
.
Done
```
```SQL
> show series
```
note auto breaking by tags
```text
key
---
testing
testing,host=sergueik53,region=region
testing,host=sergueik71,region=region

```
```SQL
> select * from testing
```

```text
name: testing
time                idle system usertime
----                ---- ------ --------
time                host       idle region system usertime
----                ----       ---- ------ ------ --------
1654382144028000000            90          1      9
1654382144140000000 sergueik53 90   region 1      9
1654382144156000000 sergueik53 90   region 1      9
1654382144157000000 sergueik53 90   region 1      9
1654382144158000000 sergueik53 90   region 1      9
1654382144159000000 sergueik53 90   region 1      9
1654382144160000000 sergueik53 90   region 1      9
1654382144161000000 sergueik53 90   region 1      9
1654382144163000000 sergueik53 90   region 1      9
1654382144164000000 sergueik53 90   region 1      9
1654382144165000000 sergueik53 90   region 1      9

```
### Spring Java Client
```cmd
mvn spring-boot:run
```
```sh
curl -H "Content-Type: application/json" -X POST -d '{"current": 1,"power":2,"voltage":2, "time":"2022-06-04T20:23:28.295+00:00"}' http://localhost:8080/influxdb/insert/
```
the client receives a success response
```text
{"success":true,"code":200,"message":"request succeeded","localizedMsg":"request succeeded","data":null}
```

however the operation is failing and the stack trace is indicating very generic misconfiguration:
```text
2022-06-04 17:10:30.222 ERROR 728 --- [8.0.29:8086/...] c.i.client.write.events.
WriteErrorEvent  : The error occurred during writing of data

com.influxdb.exceptions.NotFoundException: null
  at com.influxdb.internal.AbstractRestClient.responseToError(AbstractRestClient.java:120) ~[influxdb-client-core-4.0.0.jar:4.0.0]
  at com.influxdb.internal.AbstractRestClient.toInfluxException(AbstractRestClient.java:98) ~[influxdb-client-core-4.0.0.jar:4.0.0]
  at com.influxdb.client.internal.AbstractWriteClient.lambda$new$12(AbstractWriteClient.java:182) [influxdb-client-java-4.0.0.jar:4.0.0]
  at io.reactivex.internal.subscribers.LambdaSubscriber.onNext(LambdaSubscriber.java:65) ~[rxjava-2.2.19.jar:na]
  at io.reactivex.internal.operators.flowable.FlowableDoFinally$DoFinallySubscriber.onNext(FlowableDoFinally.java:84) ~[rxjava-2.2.19.jar:na]
  at io.reactivex.internal.operators.mixed.FlowableConcatMapMaybe$ConcatMapMaybeSubscriber.drain(FlowableConcatMapMaybe.java:284) ~[rxjava-2.2.19.jar:na]
  at io.reactivex.internal.operators.mixed.FlowableConcatMapMaybe$ConcatMapMaybeSubscriber.innerSuccess(FlowableConcatMapMaybe.java:179) ~[rxjava-2.2.19.jar:na]
  at io.reactivex.internal.operators.mixed.FlowableConcatMapMaybe$ConcatMapMaybeSubscriber$ConcatMapMaybeObserver.onSuccess(FlowableConcatMapMaybe.java:322) ~[rxjava-2.2.19.jar:na]
  at io.reactivex.internal.operators.maybe.MaybeOnErrorNext$OnErrorNextMaybeObserver$NextMaybeObserver.onSuccess(MaybeOnErrorNext.java:134) ~[rxjava-2.2.19.jar:na]
  at io.reactivex.internal.operators.maybe.MaybeJust.subscribeActual(MaybeJust.java:36) ~[rxjava-2.2.19.jar:na]
  at io.reactivex.Maybe.subscribe(Maybe.java:4290) ~[rxjava-2.2.19.jar:na]
  at io.reactivex.internal.operators.maybe.MaybeOnErrorNext$OnErrorNextMaybeObserver.onError(MaybeOnErrorNext.java:109) ~[rxjava-2.2.19.jar:na]
  at io.reactivex.internal.operators.maybe.MaybePeek$MaybePeekObserver.onErrorInner(MaybePeek.java:147) ~[rxjava-2.2.19.jar:na]
  at io.reactivex.internal.operators.maybe.MaybePeek$MaybePeekObserver.onError(MaybePeek.java:134) ~[rxjava-2.2.19.jar:na]
  at io.reactivex.internal.operators.maybe.MaybeMap$MapMaybeObserver.onError(MaybeMap.java:94) ~[rxjava-2.2.19.jar:na]
  at io.reactivex.internal.operators.maybe.MaybeTimeoutMaybe$TimeoutMainMaybeObserver.onError(MaybeTimeoutMaybe.java:106) ~[rxjava-2.2.19.jar:na]
  at io.reactivex.internal.operators.flowable.FlowableSingleMaybe$SingleElementSubscriber.onError(FlowableSingleMaybe.java:89) ~[rxjava-2.2.19.jar:na]
  at io.reactivex.subscribers.SerializedSubscriber.onError(SerializedSubscriber.java:142) ~[rxjava-2.2.19.jar:na]
  at io.reactivex.internal.operators.flowable.FlowableRepeatWhen$WhenReceiver.onError(FlowableRepeatWhen.java:112) ~[rxjava-2.2.19.jar:na]
  at io.reactivex.internal.operators.flowable.FlowableFlatMap$MergeSubscriber.checkTerminate(FlowableFlatMap.java:572) ~[rxjava-2.2.19.jar:na]
  at io.reactivex.internal.operators.flowable.FlowableFlatMap$MergeSubscriber.drainLoop(FlowableFlatMap.java:379) ~[rxjava-2.2.19.jar:na]
  at io.reactivex.internal.operators.flowable.FlowableFlatMap$MergeSubscriber.drain(FlowableFlatMap.java:371) ~[rxjava-2.2.19.jar:na]
  at io.reactivex.internal.operators.flowable.FlowableFlatMap$MergeSubscriber.innerError(FlowableFlatMap.java:611) ~[rxjava-2.2.19.jar:na]
  at io.reactivex.internal.operators.flowable.FlowableFlatMap$InnerSubscriber.onError(FlowableFlatMap.java:677) ~[rxjava-2.2.19.jar:na]
  at io.reactivex.internal.subscriptions.EmptySubscription.error(EmptySubscription.java:55) ~[rxjava-2.2.19.jar:na]
  at io.reactivex.internal.operators.flowable.FlowableError.subscribeActual(FlowableError.java:40) ~[rxjava-2.2.19.jar:na]
  at io.reactivex.Flowable.subscribe(Flowable.java:14935) ~[rxjava-2.2.19.jar:na]
  at io.reactivex.Flowable.subscribe(Flowable.java:14882) ~[rxjava-2.2.19.jar:na]
  at io.reactivex.internal.operators.flowable.FlowableFlatMap$MergeSubscriber.onNext(FlowableFlatMap.java:163) ~[rxjava-2.2.19.jar:na]
  at io.reactivex.internal.operators.flowable.FlowableZip$ZipCoordinator.drain(FlowableZip.java:249) ~[rxjava-2.2.19.jar:na]
  at io.reactivex.internal.operators.flowable.FlowableZip$ZipSubscriber.onNext(FlowableZip.java:381) ~[rxjava-2.2.19.jar:na]
  at io.reactivex.processors.UnicastProcessor.drainFused(UnicastProcessor.java:362) ~[rxjava-2.2.19.jar:na]
  at io.reactivex.processors.UnicastProcessor.drain(UnicastProcessor.java:395) ~[rxjava-2.2.19.jar:na]
  at io.reactivex.processors.UnicastProcessor.onNext(UnicastProcessor.java:457) ~[rxjava-2.2.19.jar:na]
  at io.reactivex.processors.SerializedProcessor.onNext(SerializedProcessor.java:103) ~[rxjava-2.2.19.jar:na]
  at io.reactivex.internal.operators.flowable.FlowableRepeatWhen$WhenSourceSubscriber.again(FlowableRepeatWhen.java:171) ~[rxjava-2.2.19.jar:na]
  at io.reactivex.internal.operators.flowable.FlowableRetryWhen$RetryWhenSubscriber.onError(FlowableRetryWhen.java:76) ~[rxjava-2.2.19.jar:na]
  at io.reactivex.internal.operators.maybe.MaybeToFlowable$MaybeToFlowableSubscriber.onError(MaybeToFlowable.java:75) ~[rxjava-2.2.19.jar:na]
  at io.reactivex.internal.operators.maybe.MaybeMap$MapMaybeObserver.onSuccess(MaybeMap.java:85) ~[rxjava-2.2.19.jar:na]
  at io.reactivex.internal.operators.maybe.MaybeCreate$Emitter.onSuccess(MaybeCreate.java:73) ~[rxjava-2.2.19.jar:na]
  at com.influxdb.client.internal.AbstractWriteClient$ToWritePointsMaybe$1.onResponse(AbstractWriteClient.java:444) ~[influxdb-client-java-4.0.0.jar:4.0.0]
  at retrofit2.OkHttpCall$1.onResponse(OkHttpCall.java:161) ~[retrofit-2.9.0.jar:na]
  at okhttp3.RealCall$AsyncCall.execute(RealCall.java:174) ~[okhttp-3.14.9.jar:na]
  at okhttp3.internal.NamedRunnable.run(NamedRunnable.java:32) ~[okhttp-3.14.9.jar:na]
  at java.util.concurrent.ThreadPoolExecutor.runWorker(ThreadPoolExecutor.java:1142) ~[na:1.8.0_101]
  at java.util.concurrent.ThreadPoolExecutor$Worker.run(ThreadPoolExecutor.java:617) ~[na:1.8.0_101]
  at java.lang.Thread.run(Thread.java:745) ~[na:1.8.0_101]
```
### See Also

   * introductory [documentation](https://docs.influxdata.com/influxdb/v1.8/introduction/get-started/https://docs.influxdata.com/influxdb/v1.8/introduction/get-started/)
   * influxdb query language [documentation](https://docs.influxdata.com/influxdb/v1.7/query_language/)
   * advanced  InfluxDB client [module](https://metacpan.org/pod/InfluxDB) on CPAN
   * `LineProtocol` [documentation](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_tutorial/)
   * Prometheus endpoint provided by influxdb [documentation](https://docs.influxdata.com/influxdb/v1.8/supported_protocols/prometheus/)
   * https://github.com/JonasProgrammer/docker-influxdb
   * https://www.influxdata.com/the-best-way-to-store-collect-analyze-time-series-data/
   * https://github.com/ind9/influxdb-java - probably 2.x
   * https://github.com/influxdata/influxdb-java - official, too big
   * https://devconnected.com/how-to-create-a-database-on-influxdb-1-7-2-0/ - there apparently is a v2 / v1.x compatibility concern
documented for [backward](https://docs.influxdata.com/influxdb/v1.8/tools/api/) and for [forward](https://docs.influxdata.com/influxdb/v2.0/reference/api/influxdb-1x/)


### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
