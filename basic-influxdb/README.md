### Info

this directory contains a minimally modified `InfluxDB::Client::Simple` Perl [module](https://metacpan.org/dist/InfluxDB-Client-Simple/source/lib/InfluxDB/Client/Simple.pm)

tested interacting with an InfluxDB __1.8__ [hosted on alpine](https://hub.docker.com/r/woahbase/alpine-influxdb/). 


Note, this build allows operating via REST (but not WEB) interface:
```sh
curl --silent -X POST http://192.168.0.29:8086/query?q=show%20databases | /c/tools/jq-win64.exe  '.' -
```

```JSON
{
  "results": [
    {
      "statement_id": 0,
      "series": [
        {
          "name": "databases",
          "columns": [
            "name"
          ],
          "values": [
            [
              "_internal"
            ],
            [
              "example"
            ],
            [
              "influx_test"
            ]
          ]
        }
      ]
    }
  ]
}


```

alternatively, query through Postman

![setup Page](https://github.com/sergueik/springboot_study/blob/master/basic-influxdb/screenshots/capture-postman.png)

```sh
curl --silent -X POST "http://192.168.0.29:8086/query?q=select%20*%20from%20testing&db=example&pretty=true"
```
```json
{
    "results": [
        {
            "statement_id": 0,
            "series": [
                {
                    "name": "testing",
                    "columns": [
                        "time",
                        "CPU",
                        "Memory",
                        "Server"
                    ],
                    "values": [
                        [
                            "2022-06-05T23:44:07.529619906Z",
                            100,
                            50,
                            "SERGUEIK53"
                        ],
                        [
                            "2022-06-05T23:44:12.551612791Z",
                            100,
                            50,
                            "SERGUEIK53"
                        ],
                        [
                            "2022-06-05T23:44:37.61060614Z",
                            100,
                            50,
                            "SERGUEIK53"
                        ]
                    ]
                }
            ]
        }
    ]
}

```
NOTE: encoding agruments in command line loses prettiness:
```sh
curl --silent -X POST -v "http://192.168.0.29:8086/query" --data-urlencode "q=select * from testing" --data-urlencode "db=example" --data-urlencode "pretty=true"
```
```text
{"results":[{"statement_id":0,"series":[{"name":"testing","columns":["time","CPU","Memory","Server"],"values":[["2022-06-05T23:44:07.529619906Z",100,50,"SERGUEIK53"],["2022-06-05T23:44:12.551612791Z",100,50,"SERGUEIK53"],["2022-06-05T23:44:37.61060614Z",100,50,"SERGUEIK53"]]}]}]}
```
but with `-G` flag it works as intended
```sh
curl --silent -X POST -G "http://192.168.0.29:8086/query" --data-urlencode "q=select * from testing" --data-urlencode "db=example" --data-urlencode "pretty=true"
```
produces
```json
{
    "results": [
        {
            "statement_id": 0,
            "series": [
                {
                    "name": "testing",
                    "columns": [
                        "time",
                        "CPU",
                        "Memory",
                        "Server"
                    ],
                    "values": [
                        [
                            "2022-06-05T23:44:07.529619906Z",
                            100,
                            50,
                            "SERGUEIK53"
                        ],
                        [
                            "2022-06-05T23:44:12.551612791Z",
                            100,
                            50,
                            "SERGUEIK53"
                        ],
                        [
                            "2022-06-05T23:44:37.61060614Z",
                            100,
                            50,
                            "SERGUEIK53"
                        ]
                    ]
                }
            ]
        }
    ]
}
```

one can also create and use database via similar request:
```sh
curl --silent -X POST "http://192.168.0.29:8086/query?q=CREATE%20database%20dummy"
```
```sh
curl --silent -X POST "http://192.168.0.29:8086/query?q=SHOW%20databases"
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
Aftert the run of Perl Client test, performing both `write` and `send_data` calls, the following `series` found in the `example` database:
```text
> use example
Using database example
> show series
key
---
iterations,host=SERGUEIK53
testing,appid=BAR,env=UAT,host=sergueik71,operation=send
testing,appid=BAR,env=UAT,host=sergueik71,operation=write
testing,appid=BAZ,env=UAT,host=sergueik71,operation=send
testing,appid=BAZ,env=UAT,host=sergueik71,operation=write
testing,appid=FOO,env=UAT,host=sergueik71,operation=send
testing,appid=FOO,env=UAT,host=sergueik71,operation=write
>

```

- the caller added the `operation` to tags. Query shows:
```text
 select * from  testing
name: testing
time       appid env host       operation value
----       ----- --- ----       --------- -----
1655256772 BAR   UAT sergueik71 send      42
1655256772 BAR   UAT sergueik71 write     42
1655256772 BAZ   UAT sergueik71 send      42
1655256772 BAZ   UAT sergueik71 write     42
1655256772 FOO   UAT sergueik71 send      42
1655256772 FOO   UAT sergueik71 write     42
1655256774 BAR   UAT sergueik71 send      42
...
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

### Line Protocol

* the format is very elementary
```text
measurement|,tag_set| |field_set| |timestamp
```
* measurement and tag set are separated by a comma and no spaces

* key-value pairs are joined with an equals sign = and no spaces
```text
<field_key>=<field_value>
```
* separate multiple tag-value pairs are joined with a comma and no spaces
```text
<tag_key>=<tag_value>,<tag_key>=<tag_value>
```
* the string field values are always expected to be double quoted
* separate the field set and the optional timestamp with a whitespace (required by the protocol)
* the timestamp for data point in nanosecond-precision Unix time unless

```text
```

### Example Using Perl to Nanosecond Timestamp
```sh
perl -MTime::HiRes -e 'use Time::HiRes qw( gettimeofday); my ($s, $n) = gettimeofday(); print $s.$n. $/;my $t = time(); print $t. $/'
```
```text
1655244130852723
1655244130
```
### See Also

   * introductory [documentation](https://docs.influxdata.com/influxdb/v1.8/introduction/get-started/https://docs.influxdata.com/influxdb/v1.8/introduction/get-started/)
   * influxdb query language [documentation](https://docs.influxdata.com/influxdb/v1.7/query_language/)
   * advanced  InfluxDB client [module](https://metacpan.org/pod/InfluxDB) on CPAN
   * InfluxDB `LineProtocol` [tutorial](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_tutorial/)
   * Prometheus endpoint provided by influxdb [documentation](https://docs.influxdata.com/influxdb/v1.8/supported_protocols/prometheus/)
   * https://github.com/JonasProgrammer/docker-influxdb
   * https://www.influxdata.com/the-best-way-to-store-collect-analyze-time-series-data/
   * https://github.com/ind9/influxdb-java - probably 2.x
   * https://github.com/influxdata/influxdb-java - official, too big
   * https://devconnected.com/how-to-create-a-database-on-influxdb-1-7-2-0/ - there apparently is a v2 / v1.x compatibility concern
documented for [backward](https://docs.influxdata.com/influxdb/v1.8/tools/api/) and for [forward](https://docs.influxdata.com/influxdb/v2.0/reference/api/influxdb-1x/)
  * [intro](https://habr.com/ru/company/selectel/blog/245515/) to TSDB, and InfluxDB (in Russian, with a number of valuable comments in discussion)
  * InfluxDB Grafana data source [documentation](https://grafana.com/docs/grafana/latest/datasources/influxdb/) - note this covers InfluxQL (classic InfluxDB query) separately from [Flux](https://grafana.com/docs/grafana/latest/datasources/influxdb/influxdb-flux/) query language which apparently is supported but not required
   * another InfluxDB Perl [module](https://metacpan.org/pod/InfluxDB) - JSON only (deprecated)
   * an InfluxDB LineProtocol Perl [module](https://metacpan.org/pod/InfluxDB::LineProtocol)


### Youtube Links


  * [Nodered to InfluxDB to Grafana 2022](https://www.youtube.com/watch?v=HicgjmmL-T8)
  * [Influxdb Querying Data with Powershell](https://www.youtube.com/watch?v=z7Y20toBjJs)
  * [InfluxDB 2.0 - Complete Guide to Getting Started with InfluxDB 2](https://www.youtube.com/watch?v=-gF-Jsk85bQ)
  * [Flux query language and Influxdb basics](https://www.youtube.com/watch?v=sjfBPPBw8k8)
  * [Open Source Historian: Grafana & InfluxDB](https://www.youtube.com/watch?v=BBcj-ZoufMw)
  * [Intro to Time Series Databases & Data](https://www.youtube.com/watch?v=OoCsY8odmpM)
  * [How to turn Powershell data into dashboards with Influxdb and Grafana](https://www.youtube.com/watch?v=fVzkXwAV8hg)
  * [How to instal Grafana on Ubuntu 16.04 and pull in data from InfluxDB](https://www.youtube.com/watch?v=oexrAKLQ_LI)
  * [InfluxDB and Grafana: Installation and setup](https://www.youtube.com/watch?v=siyIExDV0fw)
  * https://github.com/ypvillazon/spring-boot-metrics-to-influxdb


### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
	
