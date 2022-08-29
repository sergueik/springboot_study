### Info
this directory contains a replica of
[Prometheu4j](https://github.com/anhdat/prometheus4j) project representing pure Java wrapper of Prometheus API [spec(https://prometheus.io/docs/prometheus/latest/querying/api/) with minimal modifications

### Usage
* on  Docker host pull pinned image
```sh
docker image pull prom/prometheus:v2.27.0
```
``` text
v2.27.0: Pulling from prom/prometheus
Digest: sha256:d1a9a86b9a3e60a9ea3cde141bdc936847456acc497e0affe7e288234383efa5
Status: Image is up to date for prom/prometheus:v2.27.0
docker.io/prom/prometheus:v2.27.0
```


* run with default configuration (it will monitor itself)
```sh
docker run -p 9090:9090 -d prom/prometheus:v2.27.0
```
* run java test pointing it to prometheus host via commandline property `prometheus`:

```sh
mvn -Dprometheus=192.168.0.64 test
```
```text
-------------------------------------------------------
 T E S T S
-------------------------------------------------------
Running example.PrometheusApiClientTest
response = null
response = example.models.VectorResponse@58c1670b
response = example.models.VectorResponse@26a7b76d
response = example.models.MatrixResponse@29b5cd00
response = example.models.KeyValResponse@42d8062c
response = example.models.VectorResponse@cb51256
Tests run: 6, Failures: 0, Errors: 0, Skipped: 0, Time elapsed: 4.549 sec
```

### TODO

remove `retrofit` [dependency](https://mvnrepository.com/artifact/com.squareup.retrofit2/retrofit)

### See Also

 * Tools for querying a prometheus instance using PromQL [project](https://github.com/DanielMager/PrometheusQueries4J) - probably the later rewrite or evolution of the [current](https://github.com/anhdat/prometheus4j) one

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
