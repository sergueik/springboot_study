### Info

this directory contains a minimally modified `InfluxDB::Client::Simple` Perl [module](https://metacpan.org/dist/InfluxDB-Client-Simple/source/lib/InfluxDB/Client/Simple.pm)

tested interacting with an InfluxDB __1.8__ [hosted on alpine](https://hub.docker.com/r/woahbase/alpine-influxdb/). 

If switched to InfluxDB __2.x__ the only difference for [data ingestion command](https://docs.influxdata.com/influxdb/v2.2/write-data/developer-tools/influx-cli/)
will be in the `org` and `bucket` in the url instead of the `database` query param and presence of the authoriaation token header

```sh
export TOKEN=$(cat token.txt)
export ORG=testorg
export BUCKET=testbucket
export BASE_URL="http://192.168.0.92:8086"
export TIMESTAMP=$(perl -MTime::HiRes -e 'use Time::HiRes qw( gettimeofday); my ( $seconds, $microseconds ) = gettimeofday(); print $seconds . $microseconds ,"000", $/;')
curl -v --request POST "$BASE_URL/api/v2/write?org=$ORG&bucket=$BUCKET&precision=ns" --header "Authorization: Token $TOKEN" --header "Content-Type: text/plain; charset=utf-8"  --header "Accept: application/json"  --data-binary 'measurement,server=host1,env=uat,dc=west load=1.4,mem=35 1656351627692598'
```

```text

 Content-Type: text/plain; charset=utf-8
> Accept: application/json
> Content-Length: 67
> 
* upload completely sent off: 67 out of 67 bytes
< HTTP/1.1 204 No Content
< X-Influxdb-Build: OSS
< X-Influxdb-Version: v2.2.0
< Date: Fri, 24 Jun 2022 21:26:24 GMT
< 
* Connection #0 to host localhost left intact

```
NOTE - above command is just [example from documentation](https://docs.influxdata.com/influxdb/v2.0/write-data/developer-tools/api/) - not tested in current project

for the nanosecond-precision timestamp use the Perl one-liner

```sh
perl -MTime::HiRes -e 'use Time::HiRes qw( gettimeofday); my ( $seconds, $microseconds ) = gettimeofday(); print $seconds . $microseconds , "000", $/;'
```

if the credentials are not accepted (e.g. after the loss of the container)
[reset the password](https://docs.influxdata.com/influxdb/v2.3/reference/cli/influx/user/password/)
```sh
influx user password --name testuser --password password
```
or just recreate the container (the initial login will be redirected to `http://192.168.0.29:8086/onboarding/0`)


* [InfluxDB](https://metacpan.org/pod/InfluxDB) CPAN module also implements the InfluxDB query and write, accesps `username ` and `password `
* [AnyEvent::InfluxDB](https://metacpan.org/pod/AnyEvent::InfluxDB) module recognizes and supports JWT token


From the client module all it takes is pass the 

token in the `Authorization` header
```perl
if ($self->{token}) {
        $args{headers}->{Authorization} = 'Authorization: Token '. $self->{token};
    }
```    
    provide `org` and `bucket` instead of  `db`
    and change the URL path from `/write` to
        '/api/v2/write' 
    


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


Continue with configuring the connection to "Getting Started", "Data", "API Tokens":
![API Token Page](https://github.com/sergueik/springboot_study/blob/master/basic-influxdb/screenshots/capture-api-token.png)

Save the token and use in curl command above
```text
> token.txt  cat
Bg8vhiRXDFDthyQpOKFvvfnZ8y2t07bTs9pSRjOwMOlJ0x8GURacteQaZ0h0eARTSL4bmaaFNdYNwNps2hTbWw==
^D
```
it looks like base64 encoded text, but `base64` reports an error decoding it.
```sh
export TOKEN=$(cat token.txt)
export ORG=testuser
export BUCKET=testbucket
export BASE_URL="http://192.168.0.92:8086"
curl -v --request POST "$BASE_URL/api/v2/write?org=$ORG&bucket=$BUCKET&precision=s" --header "Authorization: Token $TOKEN" --header "Content-Type: text/plain; charset=utf-8" --header "Accept: application/json" --data-binary 'measurement,server=host1,env=uat,dc=west load=1.4,mem=35 1630424257'
```
```sh
export JQ=/c/tools/jq-win64.exe
```
or
```sh
export JQ=jq

```
```sh
curl -v --request GET --header "Authorization: Token $TOKEN" --header "Content-Type: text/plain; charset=utf-8" "$BASE_URL/api/v2/buckets" | $JQ '.buckets[].name'
```

will return user and system buckets:
```text
"_monitoring"
"_tasks"
"testbucket"

```
The query
```sh
curl -v --request POST "$BASE_URL/api/v2/query/analyze?orgID$ORG" --header "Authorization: Token $TOKEN" -d '{"query": "from(bucket: \"testbucket\")\n  |> range(start: 1h)\n  |> filter(fn: (r) => r[\"_measurement\"] == measurment)\n  |> filter(fn: (r) => r[\"_field\"] == \"load\" )\n  |> aggregateWindow(every: v.windowPeriod, fn: last, createEmpty: false)\n  |> yield(name: \"last\")"}'
```
is returning 
```JSON
{"code":"internal error","message":"An internal error has occurred - check server logs"}

```
with container logs showing Influx DB	 error:

```text
ts=2022-06-25T01:19:03.717302Z lvl=warn msg="internal error not returned to client" log_id=0bI56Su0000 handler=error_logger error="unknown query request type "
```
This is solved by examining the appplication source code [here](https://github.com/influxdata/influxdb/blob/master/http/query.go#L26) and [here](https://github.com/influxdata/influxdb/blob/master/http/query.go#L136) and adding the JSON parameter `"type":"flux"` in the body

```sh
curl -v --request POST "$BASE_URL/api/v2/query/analyze?orgID$ORG" --header "Authorization: Token $TOKEN" -d '{"type": "flux", "query": "from(bucket: \"testbucket\")\n  |> range(start: 1h)\n  |> filter(fn: (r) => r[\"_measurement\"] == measurment)\n  |> filter(fn: (r) => r[\"_field\"] == \"load\" )\n  |> aggregateWindow(every: v.windowPeriod, fn: last, createEmpty: false)\n  |> yield(name: \"last\")"}'
```

```
curl -v --request POST "$BASE_URL/api/v2/query/analyze?orgID$ORG" --header "Authorization: Token $TOKEN" -d '{"type": "flux", "query": "from(bucket: \"testbucket\")"}'
```
The response, though no longer an internal server error is still empty:
```JSON
{"errors":[]}

```
### Influx 1.x
With Influx __1.x__ connect to the container and in  console run `influx` command to open shell to create database:

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
perl -I . ingest.pl
```

With Influx __2.x__ one need to use FLUX to compose the query and pass the token in the `influx` query command in the container (unfinished)


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

### Dummy Data Ingestion

Lainch Grafana container linked to Influx DB one and inges some data with recognizable shape:

```sh
for cnt in $(seq 0 1 30 ); do export VALUE=$(expr $cnt \* $cnt ); echo $VALUE ; sleep 5 ; perl -I . ingest.pl -value $VALUE -now; done
```
and query it from Grafana via InfluxDB Data Source

![setup Page](https://github.com/sergueik/springboot_study/blob/master/basic-influxdb/screenshots/capture-influxdb-datasource.png)
using the query

```SQL
SELECT "value" FROM "measurement" WHERE ("appid" = 'FOO' AND "operation" = 'send') GROUP BY time(10m) ORDER BY time DESC
```
will see

![Data In Grafana](https://github.com/sergueik/springboot_study/blob/master/basic-influxdb/screenshots/capture-data-grafana.png)
NOTE: if the timestamp  were incorrectly formatted during data ingestion you may see Grafana complain `Data outside time rannge` and suggest `zoom to data`. After this done, see series and note the `1969-12-31` date in the range 

To make script ingest the timestamps, modify the loop to be
```sh
for cnt in $(seq 0 1 30 ); do export VALUE=$(expr $cnt \* $cnt ); echo $VALUE ; sleep 5 ; perl -I . ingest.pl -precision ns -value $VALUE; done
```
NOTE: it still appears to not provide time in the right format, and the same quwry as earlier, shows no data. therefore modified it to  generate the nanosecond-precision timestamp by appending 9 zeros to unix epoch. No other precision apprar working atm.


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
* the timestamp for data point in nanosecond-precision Unix time unless a different `precision` is provided 

### Example Using Perl to Nanosecond Timestamp
```sh
perl -MTime::HiRes -e 'use Time::HiRes qw( gettimeofday); my ($sec, $mil) = gettimeofday(); print $sec.$mil."000". $/;my $t = time(); print $t. $/'
```
```text
1655244130852723000
1655244130
```
### Observed Defects
when querying just inserted data via curl
```sh
HOST=192.168.0.29
PORT=8086
DATABASE=example
QUERY="SELECT * FROM \"testing\""
curl -G "http://$HOST:$PORT/query?pretty=true" --data-urlencode "db=$DATABASE" --data-urlencode "q=$QUERY"
```
```JSON
   "results": [
        {
            "statement_id": 0,
            "series": [
                {
                    "name": "testing",
                    "columns": [
                        "time",
                        "appid",
                        "env",
                        "host",
                        "idle",
                        "operation",
                        "region",
                        "system",
                        "usertime",
                        "value"
                    ],
                    "values": [
                        [
                            "1970-01-01T00:00:01.655256772Z",
                            "BAR",
                            "UAT",
                            "sergueik71",
                            null,
                            "write",
                            null,
                            null,
                            null,
                            42
                        ],

...
```
the `time` values are apparently badly miscalculated by the used version of InfluxDB. Compare to console run
```text
Connected to http://localhost:8086 version 1.7.11
InfluxDB shell version: 1.7.11
> use example
Using database example
> select * from testing
name: testing
time                appid env host       idle operation region system usertime value
----                ----- --- ----       ---- --------- ------ ------ -------- -----
V1655256772          BAR   UAT sergueik71      write                            42
1655256772          BAR   UAT sergueik71      send                             42
```

To fix this itis sufficient to get to default precision when ingesting the data :
```sh
perl -I . ingest.pl  -precision ns
```
and provising similar argument in the query (it is called `epoch` there):
```sh
curl -G "http://$HOST:$PORT/query?pretty=true&epoch=ns" --data-urlencode "db=$DATABASE" --data-urlencode "q=$QUERY"
```

it will return
```JSON
{
    "results": [
        {
            "statement_id": 0,
            "series": [
                {
                    "name": "testing",
                    "columns": [
                        "time",
                        "appid",
                        "env",
                        "host",
                        "operation",
                        "value"
                    ],
                    "values": [
                        [
                            1655949763540984,
                            "BAR",
                            "UAT",
                            "lenovo120S",
                            "send",
                            42
                        ],
                        [
                            1655949763540984,
                            "BAR",
                            "UAT",
                            "lenovo120S",
                            "write",
                            42
                        ],
                        [
                            1655949763540984,
                            "BAZ",
                            "UAT",
                            "lenovo120S",
                            "send",
                            42
                        ],
                        [
                            1655949763540984,
                            "BAZ",
                            "UAT",
                            "lenovo120S",
                            "write",
                            42
                        ],
                        [
                            1655949763540984,
                            "FOO",
                            "UAT",
                            "lenovo120S",
                            "send",
                            42
                        ],
                        [
                            1655949763540984,
                            "FOO",
                            "UAT",
                            "lenovo120S",
                            "write",
                            42
                        ]
                    ]
                }
            ]
        }
    ]
}

```
currently only default precision `ns` working acceptably.

### Testing on Alpine 
To 
run on bare bones alpine perl container there is a `InfluxDB/Client/SimpleAlpine.pm` and `ingest-alpine.pl`. The `URI` module is a dependency of Influx::Client::Simple but is pure perl and can be downloaded from [CPAN](https://metacpan.org/pod/URI).
```sh
wget https://cpan.metacpan.org/authors/id/O/OA/OALDERS/URI-5.10.tar.gz
tar zxvf URI-5.10.tar.gz
cp -R URI-5.10/lib/* .
```


```sh
IMAGE=$(docker container ls --format='{{.Names}}\t{{.Image}}'| grep 'influxdb:1.7-alpine'| awk '{print $1}')
echo $IMAGE
```
```sh
docker run -it --link $IMAGE -v $(pwd):/tmp/xxx -w /tmp/xxx alpine-perl sh
```
you will need to paste the value of `$IMAGE` in the command run in the container:
```sh
perl -I . ingest-alpine.pl  -precision ns -host $IMAGE
```

### Time Stamp Tool
it is important to send Line Protocol data with nanosecond-precision time otherwise
Grafana may auto zoom one to January 1970:

![setup Page](https://github.com/sergueik/springboot_study/blob/master/basic-influxdb/screenshots/capture-bad-timestamp-indicator.png)

```sh
perl nanosecond_convertor.pl  -debug -precision s -timestamp "$(date)"
```
```text
use caller provided timestamp Tue Jun 28 17:50:40 EDT 2022
timestamp:
1656453040000000000
```
```sh
perl nanosecond_convertor.pl  -debug -precision ns
```
```text
generating timestamp
using presision NANOSECONDS
1656453065000000000
```


### See Also

   * introductory [documentation](https://docs.influxdata.com/influxdb/v1.8/introduction/get-started)
   * [Prometheus vs. InfluxDB: A Monitoring Comparison](https://logz.io/blog/prometheus-influxdb/). Note, while the difference in default data ingestion mode (push for InfluxDB, pull for Prometheus) and in query DSLs (PromQL  of Prometheus, FLUx and InfluxQL of InfluxDB) mentioned - no detailed analyzis in this article
   * [Prometheus endpoints support in InfluxDB](https://www.influxdata.com/integration/prometheus-monitoring-tool/)
   * [article](https://www.influxdata.com/blog/influxdb-now-supports-prometheus-remote-read-write-natively/) stating the key differences between the Prometheus and InfluxDB in a somewhat advanced language: *Prometheus server is focused squarely on metrics data and is meant to be an ephemeral pull-based store and monitoring system. Meanwhile, InfluxDB is focused on time series (metrics and events) and is meant to be used either as an ephemeral data store or as a store of record for data that lives forever* ...
   * InfluxQL - the InfluxDB query language [documentation](https://docs.influxdata.com/influxdb/v1.7/query_language/)
   * advanced InfluxDB client [module](https://metacpan.org/pod/InfluxDB) on CPAN
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
  * [querying v 1.7](https://docs.influxdata.com/influxdb/v1.7/guides/querying_data/)
  * docker [formating arguments](https://docs.docker.com/config/formatting/) 
  * [influx 2.x API via Postman](https://www.influxdata.com/blog/getting-started-influxdb-2-0-api-postman/) - not quite working in Postman (variables are not propagated into steps) but the details of the requests can be useful with curl Perl or Powershell client examples
  * [prometheus/influxdb_exporter](https://github.com/prometheus/influxdb_exporter) - source tree of standalone app appearing to __Influx Telegraf__ [metric colector]() as a regular InfluxDB server server that accepts the InfluxDB time series metrics via the HTTP API and exports them via HTTP for Prometheus consumption, capable of preserving the original timestamps of the metric. The [images link](https://hub.docker.com/r/prom/influxdb-exporter). Apparently does not push data on its own. Most importantly does not allow pushing more than a single metric for every unique metric name and labels combination, thus making it impossible to bulk load histories
  * [prometheus remote read and remote write](https://prometheus.io/docs/operating/integrations/) and example [project](https://github.com/prometheus/prometheus/tree/release-2.36/documentation/examples/remote_storage/example_write_adapter)  and [source](https://github.com/prometheus/prometheus/blob/release-2.36/documentation/examples/remote_storage/remote_storage_adapter/influxdb/client.go)


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
  * [intro](https://tproger.ru/translations/influxdb-guide/) to Time Series and InfluxDB (in Russian)
  * [migration from Influx v1 to v2](https://www.sqlpac.com/en/documents/influxdb-migration-procedure-v1-v2.html)
  * [influxdata channel](https://www.youtube.com/channel/UCnrgOD6G0y0_rcubQuICpTQ)
  * [Integrating Prometheus and InfluxDB - Paul Dix, InfluxData](https://www.youtube.com/watch?v=6UjVX-RTFmo) - mentions but not elaborates on remote Prometheus read/write API ?
  
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
	
