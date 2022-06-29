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
export NAME=influxdb-exporter

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
for cnt in $(seq 5 1 10 ); do export VALUE=$(expr $cnt \* $cnt ); echo $VALUE ; sleep 1 ; perl -I . ingest-alpine.pl -timestamp "$(date)" -value $VALUE -debug --port $PORT --host $HOST -precision ns -measurement test; done
```
is run, one will find just one prometheus entry exposed to prometheus server scraper, the history will be lost

```text
test{appid="BAR",env="UAT",host="lenovo120S",operation="write"} 100 1656542533000
```

by increasing the delay in the loop one can see historic data

### See Also

  * [prometheus remote read and remote write](https://prometheus.io/docs/operating/integrations/)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
