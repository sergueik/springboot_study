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
### Test in Container

NOTE: The default shell in [alpine](https://alpinelinux.org) is not bash an `ps` does not show `%CPU`

* create a vanilla container
```sh
docker run --link pushgateway -v $(pwd):/tmp -w /tmp  -it alpine:3.9.5 sh
```
* in the container, add `curl`.
```
apk add curl
```
* publish metrics
```
./cpu_usage.sh
```
it will log to console
```text
cpu_usage{process="sh", pid="1"} 1592
 cpu_usage{process="vi", pid="517"} 1532
 cpu_usage{process="cpu_usage.sh", pid="1238"} 1588
 cpu_usage{process="ps", pid="1321"} 1516

```
* leave and remove the container
```
docker container prune -f
```
* verify the metrics	

```sh
curl -s http://localhost:9091/metrics | grep -i cpu_u
```
```text
# TYPE cpu_usage untyped
cpu_usage{instance="9bc14c8d2fd5",job="cpu_usage",pid="1",process="sh"} 1592
cpu_usage{instance="9bc14c8d2fd5",job="cpu_usage",pid="1238",process="cpu_usage.sh"} 1588
cpu_usage{instance="9bc14c8d2fd5",job="cpu_usage",pid="2041",process="ps"} 1516
cpu_usage{instance="9bc14c8d2fd5",job="cpu_usage",pid="517",process="vi"} 1532
```



### See Also
 * [introduction to pushing metrics](https://prometheus.io/docs/instrumenting/pushing/
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
