### Info

Docker container with a replica of [prom2json](https://github.com/prometheus/prom2json) repository - a small golang tool to scrape a Prometheus client (plaintext or protocol buffer) and dump the result as JSON for legacy usage

### NOTE

docker build times ican be very long, if the build process is not correctly defined, and an extremely time consuming

```sh
RUN go mod download
```

instruction occurs too early in the `Dockerfile`. one can workaround this by pre-downloading modules to vendor and building the app in a workspace fashion, this is how it is done in this project

### Usage


* build
```sh
export IMAGE=prom2json
docker build -t $IMAGE -f Dockerfile .
```
* build and run some generic metric producing application in a sibling container. For  example the one in the `../basic-prometheus-counter` (a java app explorting its port __8080__ to the host and publishing the system metrics in `http://localhost:8080/actuator/prometheus`:
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
# HELP jvm_threads_daemon_threads The current number of live daemon threads
# TYPE jvm_threads_daemon_threads gauge
jvm_threads_daemon_threads 16.0
# HELP tomcat_sessions_active_max_sessions
# TYPE tomcat_sessions_active_max_sessions gauge
tomcat_sessions_active_max_sessions 0.0
# HELP jvm_buffer_total_capacity_bytes An estimate of the total capacity of the buffers in this pool
# TYPE jvm_buffer_total_capacity_bytes gauge
jvm_buffer_total_capacity_bytes{id="direct",} 32768.0
jvm_buffer_total_capacity_bytes{id="mapped",} 0.0
# HELP jvm_memory_max_bytes The maximum amount of memory in bytes that can be used for memory management
# TYPE jvm_memory_max_bytes gauge
jvm_memory_max_bytes{area="heap",id="PS Survivor Space",} 9437184.0
jvm_memory_max_bytes{area="heap",id="PS Old Gen",} 6.66894336E8
jvm_memory_max_bytes{area="heap",id="PS Eden Space",} 3.12999936E8
jvm_memory_max_bytes{area="nonheap",id="Metaspace",} -1.0
jvm_memory_max_bytes{area="nonheap",id="Code Cache",} 2.5165824E8
jvm_memory_max_bytes{area="nonheap",id="Compressed Class Space",} 1.073741824E9
# HELP jvm_gc_live_data_size_bytes Size of old generation memory pool after a full GC
# TYPE jvm_gc_live_data_size_bytes gauge
jvm_gc_live_data_size_bytes 1.3927848E7
# HELP process_cpu_usage The "recent cpu usage" for the Java Virtual Machine process
# TYPE process_cpu_usage gauge
process_cpu_usage 0.002048131080389145
# HELP jvm_threads_live_threads The current number of live threads including both daemon and non-daemon threads
# TYPE jvm_threads_live_threads gauge
jvm_threads_live_threads 20.0
# HELP tomcat_sessions_active_current_sessions
# TYPE tomcat_sessions_active_current_sessions gauge
tomcat_sessions_active_current_sessions 0.0
# HELP system_cpu_usage The "recent cpu usage" for the whole system
# TYPE system_cpu_usage gauge
system_cpu_usage 0.005120327700972862
# HELP jvm_gc_pause_seconds Time spent in GC pause
# TYPE jvm_gc_pause_seconds summary
jvm_gc_pause_seconds_count{action="end of major GC",cause="Metadata GC Threshold",} 1.0
jvm_gc_pause_seconds_sum{action="end of major GC",cause="Metadata GC Threshold",} 0.203
jvm_gc_pause_seconds_count{action="end of minor GC",cause="Metadata GC Threshold",} 1.0
jvm_gc_pause_seconds_sum{action="end of minor GC",cause="Metadata GC Threshold",} 0.039
# HELP jvm_gc_pause_seconds_max Time spent in GC pause
# TYPE jvm_gc_pause_seconds_max gauge
jvm_gc_pause_seconds_max{action="end of major GC",cause="Metadata GC Threshold",} 0.0
jvm_gc_pause_seconds_max{action="end of minor GC",cause="Metadata GC Threshold",} 0.0
# HELP tomcat_sessions_expired_sessions_total
# TYPE tomcat_sessions_expired_sessions_total counter
tomcat_sessions_expired_sessions_total 0.0
# HELP jvm_classes_loaded_classes The number of classes that are currently loaded in the Java virtual machine
# TYPE jvm_classes_loaded_classes gauge
jvm_classes_loaded_classes 6955.0
# HELP jvm_gc_memory_allocated_bytes_total Incremented for an increase in the size of the young generation memory pool after one GC to before the next
# TYPE jvm_gc_memory_allocated_bytes_total counter
jvm_gc_memory_allocated_bytes_total 1.08488024E8
# HELP jvm_buffer_memory_used_bytes An estimate of the memory that the Java virtual machine is using for this buffer pool
# TYPE jvm_buffer_memory_used_bytes gauge
jvm_buffer_memory_used_bytes{id="direct",} 32768.0
jvm_buffer_memory_used_bytes{id="mapped",} 0.0
# HELP process_files_max_files The maximum file descriptor count
# TYPE process_files_max_files gauge
process_files_max_files 1048576.0
# HELP jvm_memory_committed_bytes The amount of memory in bytes that is committed for the Java virtual machine to use
# TYPE jvm_memory_committed_bytes gauge
jvm_memory_committed_bytes{area="heap",id="PS Survivor Space",} 9437184.0
jvm_memory_committed_bytes{area="heap",id="PS Old Gen",} 4.2991616E7
jvm_memory_committed_bytes{area="heap",id="PS Eden Space",} 2.45366784E8
jvm_memory_committed_bytes{area="nonheap",id="Metaspace",} 3.9763968E7
jvm_memory_committed_bytes{area="nonheap",id="Code Cache",} 9175040.0
jvm_memory_committed_bytes{area="nonheap",id="Compressed Class Space",} 5160960.0
# HELP jvm_threads_peak_threads The peak live thread count since the Java virtual machine started or peak was reset
# TYPE jvm_threads_peak_threads gauge
jvm_threads_peak_threads 20.0
# HELP jvm_gc_max_data_size_bytes Max size of old generation memory pool
# TYPE jvm_gc_max_data_size_bytes gauge
jvm_gc_max_data_size_bytes 6.66894336E8
# HELP jvm_gc_memory_promoted_bytes_total Count of positive increases in the size of the old generation memory pool before GC to after GC
# TYPE jvm_gc_memory_promoted_bytes_total counter
jvm_gc_memory_promoted_bytes_total 7573448.0
# HELP system_cpu_count The number of processors available to the Java virtual machine
# TYPE system_cpu_count gauge
system_cpu_count 2.0
# HELP tomcat_sessions_rejected_sessions_total
# TYPE tomcat_sessions_rejected_sessions_total counter
tomcat_sessions_rejected_sessions_total 0.0
# HELP logback_events_total Number of error level events that made it to the logs
# TYPE logback_events_total counter
logback_events_total{level="warn",} 0.0
logback_events_total{level="debug",} 0.0
logback_events_total{level="error",} 0.0
logback_events_total{level="trace",} 0.0
logback_events_total{level="info",} 7.0
# HELP jvm_buffer_count_buffers An estimate of the number of buffers in the pool
# TYPE jvm_buffer_count_buffers gauge
jvm_buffer_count_buffers{id="direct",} 4.0
jvm_buffer_count_buffers{id="mapped",} 0.0
# HELP process_start_time_seconds Start time of the process since unix epoch.
# TYPE process_start_time_seconds gauge
process_start_time_seconds 1.648767214224E9
# HELP process_files_open_files The open file descriptor count
# TYPE process_files_open_files gauge
process_files_open_files 25.0
# HELP tomcat_sessions_alive_max_seconds
# TYPE tomcat_sessions_alive_max_seconds gauge
tomcat_sessions_alive_max_seconds 0.0
# HELP process_uptime_seconds The uptime of the Java virtual machine
# TYPE process_uptime_seconds gauge
process_uptime_seconds 879.0
# HELP system_load_average_1m The sum of the number of runnable entities queued to available processors and the number of runnable entities running on the available processors averaged over a period of time
# TYPE system_load_average_1m gauge
system_load_average_1m 0.009765625
# HELP http_server_requests_seconds
# TYPE http_server_requests_seconds summary
http_server_requests_seconds_count{exception="None",method="GET",outcome="SUCCESS",status="200",uri="/actuator/prometheus",} 1.0
http_server_requests_seconds_sum{exception="None",method="GET",outcome="SUCCESS",status="200",uri="/actuator/prometheus",} 0.061467204
http_server_requests_seconds_count{exception="None",method="GET",outcome="SUCCESS",status="200",uri="/",} 1.0
http_server_requests_seconds_sum{exception="None",method="GET",outcome="SUCCESS",status="200",uri="/",} 0.109231829
http_server_requests_seconds_count{exception="None",method="GET",outcome="CLIENT_ERROR",status="404",uri="/**",} 1.0
http_server_requests_seconds_sum{exception="None",method="GET",outcome="CLIENT_ERROR",status="404",uri="/**",} 0.01536888
# HELP http_server_requests_seconds_max
# TYPE http_server_requests_seconds_max gauge
http_server_requests_seconds_max{exception="None",method="GET",outcome="SUCCESS",status="200",uri="/actuator/prometheus",} 0.061467204
http_server_requests_seconds_max{exception="None",method="GET",outcome="SUCCESS",status="200",uri="/",} 0.109231829
http_server_requests_seconds_max{exception="None",method="GET",outcome="CLIENT_ERROR",status="404",uri="/**",} 0.01536888
# HELP jvm_threads_states_threads The current number of threads having NEW state
# TYPE jvm_threads_states_threads gauge
jvm_threads_states_threads{state="runnable",} 6.0
jvm_threads_states_threads{state="blocked",} 0.0
jvm_threads_states_threads{state="waiting",} 12.0
jvm_threads_states_threads{state="timed-waiting",} 2.0
jvm_threads_states_threads{state="new",} 0.0
jvm_threads_states_threads{state="terminated",} 0.0

```
* collect just the metric names
```sh
docker run --rm  $IMAGE http://192.168.0.64:8080/actuator/prometheus | jq '.[].name'| sort
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
tomcat_sessions_created_sessions_total
tomcat_sessions_expired_sessions_total
tomcat_sessions_rejected_sessions_total
```
or specific metric names
```sh
docker run --rm  $IMAGE http://192.168.0.64:8080/actuator/prometheus | jq -cr '.[].name|select(.| contains("jvm"))
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
or specific entries selected by metric name:
```sh
docker run --rm  $IMAGE http://192.168.0.64:8080/actuator/prometheus | jq -r '.|.[]|select(.name| contains("system_load_average"))'
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
docker container rm -f $NAME
docker container prune -f
docker image rm $IMAGE
docker image rm prom/prom2json
docker image rm prom/prom2json:v1.3.0
docker image rm prom/prometheus:v2.27.0
```
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
~

