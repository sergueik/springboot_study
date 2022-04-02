### Info

Few bash and shell scripts to publish metrics to pushgateway

### Usage
* launch pushgateway container publishing its port `9091` to localhost `9091`
```sh
NAME=pushgateway
IMAGE=pushgateway
docker build -f Dockerfile.$IMAGE -t $IMAGE . 
docker container rm -f pushgateway
docker run --name $NAME -p 9091:9091 -d $IMAGE
```
* send metrics
```sh
./cpu_usage.sh  localhost
```
* show metrics on pushgateway
```sh
curl -s http://localhost:9091/metrics | grep -i cpu_u
```
```text
# TYPE cpu_usage untyped
cpu_usage{instance="lenovoy40-1",job="cpu_usage",pid="1",process="/sbin/init"} 0
cpu_usage{instance="lenovoy40-1",job="cpu_usage",pid="2",process="[kthreadd]"} 0
cpu_usage{instance="lenovoy40-1",job="cpu_usage",pid="4",process="[kworker/0:0H]"} 0
cpu_usage{instance="lenovoy40-1",job="cpu_usage",pid="6",process="[mm_percpu_wq]"} 0
push_failure_time_seconds{instance="lenovoy40-1",job="cpu_usage"} 0
push_time_seconds{instance="lenovoy40-1",job="cpu_usage"} 1.6488571168761053e+09
```
### See Also
 * [introduction to pushing metrics](https://prometheus.io/docs/instrumenting/pushing/
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
