### Info

Docker container with a replica of [prom2json](https://github.com/prometheus/prom2json) repository - a small golang tool to scrape a Prometheus client (plaintext or protocol buffer) and dump the result as JSON for legacy usage

### NOTE

docker build times ican be very long, if the build process is not correctly defined.
The [image](https://hub.docker.com/r/prom/prom2json) on docker hub is tiny: 6 Mb compressed.
The purpose of this repository is to build and test customized exporters


```sh
RUN go mod download
```
instruction appears too time-comsuming and commented from the `Dockerfile`.
The workaround is copy pre-downloading modules `vendor` directory
and build the appplication in a workspace fashion.
This is how it is done in this project

### Usage


* build
```sh
export IMAGE=prom2json
docker build -t $IMAGE -f Dockerfile .
```
* build and run some generic [metric producing application](https://github.com/sergueik/springboot_study/tree/master/basic-prometheus-counter)
a java app explorting its port __8080__ to the host and
publishing the system metrics in `http://localhost:8080/actuator/prometheus`
in a sibling container. After the continer is started one will be able to see metrics on the host:

```sh
curl http://localhost:8080/actuator/prometheus
```
```text
# HELP jvm_classes_unloaded_classes_total The total number of classes unloaded since the Java virtual machine has started execution
# TYPE jvm_classes_unloaded_classes_total counter
jvm_classes_unloaded_classes_total 0.0
# HELP tomcat_sessions_created_sessions_total
# TYPE tomcat_sessions_created_sessions_total counter
tomcat_sessions_created_sessions_total 0.0
# HELP jvm_memory_used_bytes The amount of used memory
# TYPE jvm_memory_used_bytes gauge
jvm_memory_used_bytes{area="heap",id="PS Survivor Space",} 0.0
jvm_memory_used_bytes{area="heap",id="PS Old Gen",} 1.3927848E7
jvm_memory_used_bytes{area="heap",id="PS Eden Space",} 5.4688832E7
jvm_memory_used_bytes{area="nonheap",id="Metaspace",} 3.7061848E7
jvm_memory_used_bytes{area="nonheap",id="Code Cache",} 8681344.0
jvm_memory_used_bytes{area="nonheap",id="Compressed Class Space",} 4615144.0
...
```
* collect, convert to JSON and filter with jq reporting just the metric names
accessing the host from the container:
```sh
docker run --rm $IMAGE http://172.17.0.1:8080/actuator/prometheus | jq '.[].name'| sort
```

```text
http_server_requests_seconds
http_server_requests_seconds_max
jvm_buffer_count_buffers
jvm_buffer_memory_used_bytes
jvm_buffer_total_capacity_bytes
jvm_classes_loaded_classes
jvm_classes_unloaded_classes_total
jvm_gc_live_data_size_bytes
jvm_gc_max_data_size_bytes
jvm_gc_memory_allocated_bytes_total
jvm_gc_memory_promoted_bytes_total
jvm_gc_pause_seconds
jvm_gc_pause_seconds_max
jvm_memory_committed_bytes
jvm_memory_max_bytes
jvm_memory_used_bytes
jvm_threads_daemon_threads
jvm_threads_live_threads
jvm_threads_peak_threads
jvm_threads_states_threads
logback_events_total
process_cpu_usage
process_files_max_files
process_files_open_files
process_start_time_seconds
process_uptime_seconds
system_cpu_count
system_cpu_usage
system_load_average_1m
tomcat_sessions_active_current_sessions
tomcat_sessions_active_max_sessions
tomcat_sessions_alive_max_seconds
tomcat_sessions_create d_sessions_total
tomcat_sessions_expired_sessions_total
tomcat_sessions_rejected_sessions_total
```
or specific metric names
```sh
docker run --rm $IMAGE http://172.17.0.1:8080/actuator/prometheus | jq -cr '.[].name|select(.| contains("jvm"))'
```
can also use external ip of the host (`192.168.0.119` in the command below):

```sh
docker run --rm $IMAGE http://192.168.0.119:8080/actuator/prometheus | jq -cr '.[].name|select(.| contains("jvm"))'
```
```text
jvm_threads_daemon_threads
jvm_memory_committed_bytes
jvm_threads_states_threads
jvm_gc_live_data_size_bytes
jvm_threads_peak_threads
jvm_classes_unloaded_classes_total
jvm_gc_memory_promoted_bytes_total
jvm_buffer_count_buffers
jvm_threads_live_threads
jvm_gc_memory_allocated_bytes_total
jvm_memory_used_bytes
jvm_memory_max_bytes
jvm_classes_loaded_classes
jvm_gc_max_data_size_bytes
jvm_buffer_total_capacity_bytes
jvm_gc_pause_seconds
jvm_gc_pause_seconds_max
jvm_buffer_memory_used_bytes

```
or specific metric data selected by metric name:
```sh
docker run --rm  $IMAGE http://172.17.0.1:8080/actuator/prometheus | jq -r '.|.[]|select(.name| contains("system_load_average"))'
```
```json
{
  "name": "system_load_average_1m",
  "help": "The sum of the number of runnable entities queued to available processors and the number of runnable entities running on the available processors averaged over a period of time",
  "type": "GAUGE",
  "metrics": [
    {
      "value": "0.61279296875"
    }
  ]
}
```
### Note: 

Application does require go version __1.13__ in the HEAD revision. Switching to `golang:alpine3.9`
(go version __1.12__) leads to compilation error:

```sh
# command-line-arguments
cmd/prom2json/main.go:114:54: http.DefaultTransport.(*http.Transport).Clone undefined (type *http.Transport has no field or method Clone)
note: module requires Go 1.13

```
```sh
docker image rm $IMAGE
docker image rm prom/prom2json
docker image rm prom/prom2json:v1.3.0
docker image rm prom/prometheus:v2.27.0
```
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
