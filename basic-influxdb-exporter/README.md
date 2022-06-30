### Info

this directory contains project using [prometheus influxdb-exporter](https://github.com/prometheus/influxdb_exporter)

to re-feed the historic time series to Prometheus by acting like an InfluxDB server to a custom Perl or Java app
while dumping the received data points as a valid Prometheus  metrics to Prometheus to scrape and later provide the same to Grafana

The  source code of the vendor project are saved locally, but the custom server is not  being build from source

### Usage
pull vendor image
```sh
VERSION=v0.9.1
export IMAGE=prom/influxdb-exporter:$VERSION
docker pull $IMAGE 
```

run overriding the `ENTRYPOINT`
```sh
ENTRYPOINT  [ "/bin/influxdb_exporter" ]
```
adding the vital `--timestamps` and optional `--influxdb.sample-expiry` flag arguments to export genuine timestamps of points and too keep thus unblocking the sender 
```sh
export NAME=influxdb_exporter
export ENTRYPOINT='/bin/influxdb_exporter --timestamps --influxdb.sample-expiry 10m'
docker run -d -p 9122:9122 --name $NAME --entrypoint "$ENTRYPOINT" $IMAGE  
```

the correct way is actually provide arguments to be merged explicitly:

```sh
docker run -d -p 9122:9122 --name $NAME $IMAGE --timestamps --influxdb.sample-expiry 10m
```
feed the data
```sh
pushd ../basic-influxdb
export PORT=9122
export HOST=localhost
for cnt in $(seq 5 1 10 ); do export VALUE=$(expr $cnt \* $cnt ); echo $VALUE ; sleep 1 ; perl -I . ingest-alpine.pl -timestamp "$(date)" -value $VALUE -debug --port $PORT --host $HOST -precision ns -measurement test; done
popd
```
note it will not output as much debugging info as the real InfluxDB server
you will see
```text
Raw content
$VAR1 = '{"results": []}';
Decoded content
$VAR1 = {
          'results' => []
        };
$VAR1 = {
          'request_id' => undef,
          'results' => []
        };
Result for send:
$VAR1 = undef;
```
repeatedly

The data will appear on static metrics page `http://192.168.0.29:9122/metrics`:
```text
# HELP exported InfluxDB Metric
# TYPE exported untyped
exported{appid="BAR",env="UAT",host="lenovo120S",operation="write"} 100 1656541996000
exported{appid="BAZ",env="UAT",host="lenovo120S",operation="write"} 100 1656541996000
exported{appid="FOO",env="UAT",host="lenovo120S",operation="write"} 100 1656541996000
```
but there will not be a lot of data,
Prometheus will not allow more than one metric with same unique metric and label combinations:
later will wipe earlier points
after the example loop
```sh
for cnt in $(seq 5 1 10 ); do export VALUE=$(expr $cnt \* $cnt ); echo $VALUE ; sleep 1 ; POINT_DATE=$(date); POINT_DATE=$(date +"%c" -d "${POINT_DATE} - 1 hour") ; perl -I . ingest.pl -timestamp "${POINT_DATE}" -value $VALUE -debug --port $PORT --host $HOST -precision ns -measurement test; done
```
is run, one will find just one prometheus entry exposed to prometheus server scraper, the history will be lost

```text
test{appid="BAR",env="UAT",host="lenovo120S",operation="write"} 100 1656542533000
```

by increasing the delay in the loop one can see historic data

### Confirming the Timestamp

* pull prometheus image
```sh
export 
```sh
VERSION=v2.27.0
export IMAGE=prom/prometheus:$VERSION
docker pull $IMAGE
```
* add or update the configuration file `prometheus.yml` listing `influx_exporter` host:
```yaml
scrape_configs:
  - job_name: 'influx_exporter'

    scrape_interval: 1s
    metrics_path: /metrics
    honor_labels: true

    static_configs:
      - targets: ['influx_exporter:9122']
        labels:
          group: 'influx_exporter
```
note  the scrape interval is set to very low to avoid dealing with the data expiring before being scraped
* run prometheus node linked to and configured to scrape the `influxdb_exporter` 
with the additional [option flag](https://www.robustperception.io/reloading-prometheus-configuration)
required to enable the `http://localhost:9090/-/reload` endpoint
```sh
docker run --link influxdb_exporter -p 9090:9090 -v $(pwd)/prometheus.yml:/etc/prometheus/prometheus.yml $IMAGE --web.enable-lifecycle --config.file=/etc/prometheus/prometheus.yml
```
note that together with `web.enable-lifecycle` one has to provide the `config.file` argument, to avoid the error
```text
msg="Error loading config (--config.file=prometheus.yml)" err="open prometheus.yml: no such file or directory"
```
NOTE: with `+"%c"` get the format parser is not able to 
```text
use caller provided timestamp Thu 30 Jun 2022 04:10:09 PM EDT
```

need
```text
+"%a %b %d %H:%M:%S %Z %Y"
```
```sh

for cnt in $(seq 5 1 10 ); do export VALUE=$(expr $cnt \* $cnt ); echo $VALUE ; sleep 1 ; POINT_DATE=$(date); POINT_DATE=$(date +"%a %b %d %H:%M:%S %Z %Y" -d "${POINT_DATE} - 5 minute") ; perl -I . ingest.pl -timestamp "${POINT_DATE}" -value $VALUE -debug --port $PORT --host $HOST -precision ns -measurement test; done
```

```sh
docker exec  -it  influxdb_exporter sh
```
```sh
ifconfig eth0
eth0      Link encap:Ethernet  HWaddr 02:42:AC:11:00:04
          inet addr:172.17.0.4  Bcast:172.17.255.255  Mask:255.255.0.0
          UP BROADCAST RUNNING MULTICAST  MTU:1500  Metric:1
          RX packets:232 errors:0 dropped:0 overruns:0 frame:0
          TX packets:189 errors:0 dropped:0 overruns:0 carrier:0
          collisions:0 txqueuelen:0
          RX bytes:29175 (28.4 KiB)  TX bytes:18138 (17.7 KiB
```

```sh
 docker exec -it b2697567eb5a sh
```
```sh

/prometheus $ ifconfig eth0
eth0      Link encap:Ethernet  HWaddr 02:42:AC:11:00:05
          inet addr:172.17.0.5  Bcast:172.17.255.255  Mask:255.255.0.0
          UP BROADCAST RUNNING MULTICAST  MTU:1500  Metric:1
          RX packets:402 errors:0 dropped:0 overruns:0 frame:0
          TX packets:398 errors:0 dropped:0 overruns:0 carrier:0
          collisions:0 txqueuelen:0
          RX bytes:57604 (56.2 KiB)  TX bytes:44680 (43.6 KiB)

```

when the date is too far  the past (e.g. 1 hour old while `influxdb.sample-expiry` was set to `10 minute` - no data will be added


![config](https://github.com/sergueik/springboot_study/blob/master/basic-influxdb-exporter/screenshots/capture-targets.png)

run two batches, both feeding the timestamp to avoid time zone gaps
* historic
```sh
for cnt in $(seq 1 1 5 ); do export VALUE=$(expr $cnt \* $cnt ); echo $VALUE ; sleep 1 ; POINT_DATE=$(date); POINT_DATE=$(date +"%a %b %d %H:%M:%S %Z %Y" -d "${POINT_DATE} - 3 minute") ; perl -I . ingest.pl -timestamp "${POINT_DATE}" -value $VALUE -debug --port $PORT --host $HOST -precision ns -measurement test; done
```
* recent
```sh
for cnt in $(seq 1 1 10 ); do export VALUE=$(expr $cnt \* $cnt ); echo $VALUE ; sleep 5; POINT_DATE=$(date); POINT_DATE=$(date +"%a %b %d %H:%M:%S %Z %Y" -d "${POINT_DATE}") ; perl -I . ingest.pl -timestamp "${POINT_DATE}" -value $VALUE -debug --port $PORT --host $HOST -precision ns -measurement test; done
```

![past data](https://github.com/sergueik/springboot_study/blob/master/basic-influxdb-exporter/screenshots/capture_successful_ingested_past_points.png)


![Past and more recent data](https://github.com/sergueik/springboot_study/blob/master/basic-influxdb-exporter/screenshots/capture_successful_ingested_past_then_current_points.png)

note, prometheus logs issues with samples, when the data being inserted is being older than data already ignested for same metric:
```text
level=warn ts=2022-06-30T21:53:24.938Z caller=scrape.go:1467 component="scrape manager" scrape_pool=influxdb_exporter target=http://influxdb_exporter:9122/metrics msg="Error on ingesting out-of-order samples" num_dropped=3
level=warn ts=2022-06-30T21:53:25.938Z caller=scrape.go:1467 component="scrape manager" scrape_pool=influxdb_exporter target=http://influxdb_exporter:9122/metrics msg="Error on ingesting out-of-order samples" num_dropped=3
level=warn ts=2022-06-30T21:53:26.938Z caller=scrape.go:1467 component="scrape manager" scrape_pool=influxdb_exporter target=http://influxdb_exporter:9122/metrics msg="Error on ingesting out-of-order samples" num_dropped=3
level=warn ts=2022-06-30T21:53:27.938Z caller=scrape.go:1467 component="scrape manager" scrape_pool=influxdb_exporter target=http://influxdb_exporter:9122/metrics msg="Error on ingesting out-of-order samples" num_dropped=3
level=warn ts=2022-06-30T21:53:28.939Z caller=scrape.go:1467 component="scrape manager" scrape_pool=influxdb_exporter target=http://influxdb_exporter:9122/metrics msg="Error on ingesting out-of-order samples" num_dropped=3
```

this will be the case if "now" ingestion  to be done before the "historic".
in the opposite order, both work

### See Also

  * [prometheus remote read and remote write](https://prometheus.io/docs/operating/integrations/)
  * [backfilling from OpenMetrics format](https://prometheus.io/docs/prometheus/latest/storage/#backfilling-from-openmetrics-format)
  * the `io.prometheus.client` Java adapter [source tree](https://github.com/prometheus/client_java#gauge)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

