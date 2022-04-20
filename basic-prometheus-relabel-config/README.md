### Info
this directory contains a reolica of
[prometheus relabel config](https://github.com/mrWinston/relabel-config-playground) repository
### Usage

```sh
PROMETHEUS_VERSION=v2.27.0
docker pull prom/prometheus:$PROMETHEUS_VERSION
```	
```sh
EXPORTER_VERSION=v1.3.1
docker pull prom/node-exporter:$EXPORTER_VERSION
```	
```sh
IMAGE=node-exporter
docker build -t $IMAGE -f Dockerfile.$IMAGE .
```
```sh
NAME=metrics
docker run -p 9100:9100 --name $NAME -d $IMAGE
```

```sh
curl -s $(hostname -i):9100/metrics |  grep "sample_counter"
```	
```text
# HELP sample_counter_one just a sample counter thats always 1 ( except for when you change it )
# TYPE sample_counter_one counter
sample_counter_one{lone="boo",ltwo="bar"} 1
sample_counter_one{lone="boo",ltwo="far"} 1
sample_counter_one{lone="foo",ltwo="bar"} 1
sample_counter_one{lone="foo",ltwo="far"} 1
# HELP sample_counter_two just a sample counter thats always 2
# TYPE sample_counter_two counter
sample_counter_two{lone="boo",ltwo="bar"} 2
sample_counter_two{lone="boo",ltwo="far"} 2
sample_counter_two{lone="foo",ltwo="bar"} 2
sample_counter_two{lone="foo",ltwo="far"} 2
```
repeat with `docker-compose`
```sh
export COMPOSE_HTTP_TIMEOUT=600
docker-compose up
```
### Initial State



### Relabel Exercise

* the cluster consists of `prometheus`  prometheus server and a `metrics` node. The latter runs node exporter reading metrics from static file:
```sh
/bin/node_exporter --collector.textfile.directory /tmp/metrics
```
* the `prometheus` node is using custom  prometheus.yml to collect the metrics from `metrics`
```YAML
  - job_name: "static-metrics"
    static_configs:
      - targets:
        # NOTE: port
        - "metrics:9100"
```
* replace the `__address__` with the following yaml:
```YAML
    relabel_configs:
      - source_labels: [__address__]
        regex: "metrics:(.*)"
        target_label: __address__
        replacement: 'monitored_host:$1'
        action: replace
```
This does have effect but it the effect is opposite to what was intended:

![relabel_address](https://github.com/sergueik/springboot_study/blob/master/basic-prometheus-relabel-config/screenshots/capture-relabel_address.png) 

Apparentlty Prometheus does replace the address and failed in attempt to read metrics from the new address directly - the `monitored_host` is not neither is not supposed to be resolvable

### See Also

* https://prometheus.io/docs/instrumenting/exposition_formats/#text-format-example
