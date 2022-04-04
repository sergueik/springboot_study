### Info


this directory project is a replica of project based on plain Java (non-Spring)
[prometheus pushgateway client metric push demo application](https://github.com/shejoshi/pushgateway)
of Prometheus [pushgateway](https://github.com/prometheus/pushgateway) merged with Apache contgiguration allowing controling the "job" detals through arguments
and an Alpine docker containers with selected release installed

post one fake job metrics (setting the last argument to `true` indicates if the job has failures )
```sh
java -cp target/example.pushgateway-client.jar:target/lib/* example.App -s false --name "job name" --debug
```
The Java application logs "running the job" and "processing job failure":
			
```text
Missing argument: delay, using defult
Executing job: job name with status: failure
get CollectorRegistry: 713338599
Set job duration timer: 1406718218
Set job info gauge: 245257410 [Name: job_info Type: GAUGE Help: Job identifier. Samples: []]
Exception (ignored): java.lang.NullPointerException
Executing job with status: failure
Job is failing
Job Failure processing block
Set job failure timestamp gauge: 1705736037

```
This will log in the container console
```sh
{"caller":"level.go:63","file":"/pushgateway/history.log","level":"info","msg":"metrics persisted","ts":"2021-10-27T02:57:19.671Z"}
{"caller":"level.go:63","file":"/pushgateway/history.log","level":"info","msg":"metrics persisted","ts":"2021-10-27T02:58:03.371Z"}
```

### Run with Vendor containers
### Prometheus

optionally create a separate network
```sh
export NETWORK=example_pushgateway
docker network create $NETWORK
```

```sh
PROMETHEUS_VERSION=v2.27.0
docker run -p 9090:9090 --name=prometheus -v $(pwd)/prometheus.yml:/etc/prometheus/prometheus.yml --network=$NETWORK prom/prometheus:$PROMETHEUS_VERSION
```	
#### Pushgateway

* run the `pushgateway` container explosing port `9091` to host:

```sh
PUSHGATEWAY_VERSION=v1.4.2
docker container rm pushgateway
docker run -d -p 9091:9091 --name=pushgateway --network=$NETWORK prom/pushgateway:$PUSHGATEWAY_VERSION
```

###  Run demo app locally
```sh
mvn package
```
post one fake job metrics (setting the last argument to `true` indicates if the job has failures )
```sh
for C in $(seq 1 1 5); do java -cp target/example.pushgateway-client.jar:target/lib/* example.App -s false --name "job name" --debug; sleep 10 ; done
```
* ignore the exception
```text
Missing argument: delay, using defult
Executing job: job name with status: success
get CollectorRegistry: 713338599
Set job duration timer: 1406718218
Set job info gauge: 245257410 [Name: job_info Type: GAUGE Help: Job identifier. Samples: []]
Exception (ignored): java.lang.NullPointerException
java.lang.NullPointerException
        at io.prometheus.client.Gauge.set(Gauge.java:265)
        at example.App.executeBatchJob(App.java:79)
        at example.App.main(App.java:181)
Executing job with status: success
Job complete
Set job success gauge: [Name: job_last_success Type: GAUGE Help: Last successful job run Samples: []]
Sending job info

```
* observe the metric placed:
```sh
curl -s http://localhost:9091/metrics | grep job

```
```text

# HELP job_duration_seconds Job duration in seconds.
# TYPE job_duration_seconds gauge
job_duration_seconds{instance="",job="job+name"} 3.003725464
# HELP job_info Job identifier.
# TYPE job_info gauge
job_info{instance="",job="job+name",more_info="more information",test_name="test name",test_suite="test suite"} 0
# HELP job_last_failure Last failed job run
# TYPE job_last_failure gauge
job_last_failure{instance="",job="job+name"} 1.648936468425e+09
# HELP job_last_success Last successful job run
# TYPE job_last_success gauge
job_last_success{instance="",job="job+name"} 1.648937581638e+09
push_failure_time_seconds{instance="",job="job+name"} 0
push_time_seconds{instance="",job="job+name"} 1.6489375817939155e+09
```
the pushed metrics will be visible on pushgateway web ui

![pushgateway page](https://github.com/sergueik/springboot_study/blob/master/basic-pushgateway/screenshots/pushgateway_page_capture.png)

and prometheus web ui

![prometheus page](https://github.com/sergueik/springboot_study/blob/master/basic-pushgateway/screenshots/prometheus_capture.png)

### Cleanup


```sh
docker container rm prometheus
docker container stop pushgateway
docker container rm pushgateway
docker network prune -f
```
### Windows Exporter


open [prometheus-community/windows_exporter)](https://github.com/prometheus-community/windows_exporter), navigate to  `Releases`, download the release of choice e.g. __0.18.1__
- select the msi 386 when installing in Windows 7, and 64 for Windows 10

install running in Administrator console

```cmd
mkdir "C:\temp\custom_metrics\"
```
```cmd
msiexec.exe /i C:\Users\sergueik\Downloads\windows_exporter-0.18.1-386.msi ENABLED_COLLECTORS="ad,iis,logon,memory,process,tcp,thermalzone" TEXTFILE_DIR="C:\temp\custom_metrics\"
```
or
```cmd
msiexec.exe /i C:\Users\sergueik\Downloads\windows_exporter-0.18.1-amd64.msi ENABLED_COLLECTORS="ad,iis,logon,memory,process,tcp,thermalzone" TEXTFILE_DIR="C:\temp\custom_metrics\"
```

Note, there will not be moch output
check the event viewer Application logds for: log id 1033 from msiexec

![windows exporter metrics page](https://github.com/sergueik/springboot_study/blob/master/basic-pushgateway/screenshots/eventlog_capture.png)
confirm the service
```cmd
sc.exe qc windows_exporter
```
```text
[SC] QueryServiceConfig SUCCESS

SERVICE_NAME: windows_exporter
        TYPE               : 10  WIN32_OWN_PROCESS
        START_TYPE         : 2   AUTO_START
        ERROR_CONTROL      : 1   NORMAL
        BINARY_PATH_NAME   : "C:\Program Files\windows_exporter\windows_exporter.exe" --log.format logger:eventlog?name=windows_exporter

        BINARY_PATH_NAME   : "C:\Program Files\windows_exporter\windows_exporter.exe" --log.format logger:eventlog?name=windows_exporter --collectors.enabled ad,iis,logon,memory,process,tcp,thermalzone     --collector.textfile.directory C:\temp\custom_metrics\
        LOAD_ORDER_GROUP   :
        TAG                : 0
        DISPLAY_NAME       : windows_exporter
        DEPENDENCIES       : wmiApSrv
        SERVICE_START_NAME : LocalSystem

```
there is no configuration file in "C:\Program Files\windows_exporter"

Example file exsts in https://github.com/prometheus-community/windows_exporter/blob/master/docs/example_config.yml

conirm changes in Windows firewall,enabling outbound windows_exporter HTTP endpoint


can find in running processes
```cmd
tasklist  | findstr /i exporter
```

```text
windows_exporter.exe          4160 Services                   0    22 072 КБ
```



in the browser

there is static page on `http://localhost:9182/` with the following text:
```text
windows_exporter
Metrics

(version=0.18.1, branch=heads/tags/v0.18.1, revision=e07b2053af3ca708db3489f41e2fcde9941860f4)
```

and dynamic Prometheus metrics page:
`http://localhost:9182/metrics`

![windows exporter metrics page](https://github.com/sergueik/springboot_study/blob/master/basic-pushgateway/screenshots/windows_explorer_capture.png)
the individual metrics in the default configuration vary with OS, e.g. on __Windows 10__, but not on __Windows 7__, the following is available:
```text
# HELP windows_process_cpu_time_total Returns elapsed time that all of the threads of this process used the processor to execute instructions by mode (privileged, user).
# HELP windows_process_handles Total number of handles the process has open. This number is the sum of the handles currently open by each thread in the process.
# HELP windows_process_io_bytes_total Bytes issued to I/O operations in different modes (read, write, other).
# HELP windows_process_io_operations_total I/O operations issued in different modes (read, write, other).
# HELP windows_process_page_faults_total Page faults by the threads executing in this process.
# HELP windows_process_page_file_bytes Current number of bytes this process has used in the paging file(s).
# HELP windows_process_priority_base Current base priority of this process. Threads within a process can raise and lower their own base priority relative to the process base priority of the process.
# HELP windows_process_private_bytes Current number of bytes this process has allocated that cannot be shared with other processes.
# HELP windows_process_start_time Time of process start.
# HELP windows_process_threads Number of threads currently active in this process.
# HELP windows_process_virtual_bytes Current size, in bytes, of the virtual address space that the process is using.
# HELP windows_process_working_set_bytes Maximum number of bytes in the working set of this process at any point in time. The working set is the set of memory pages touched recently by the threads in the process.
# HELP windows_process_working_set_peak_bytes Maximum size, in bytes, of the Working Set of this process at any point in time. The Working Set is the set of memory pages touched recently by the threads in the process.
# HELP windows_process_working_set_private_bytes Size of the working set, in bytes, that is use for this process only and not shared nor shareable by other processes.
```
### See Also

  * [usage documentation](https://prometheus.io/docs/instrumenting/pushing/)
  * [spring-boot example](https://github.com/ramesh-dev/prometheus-pushgateway-demo) - demonstrates configuring credentials parameters
  * https://www.reddit.com/r/docker/comments/m9l5k2/noob_question_what_is_the_difference_between/
  * Prometheus client libraries [metric types](https://prometheus.io/docs/concepts/metric_types/) - note `Info` is not covered
  * [Instrumenting Applications with Metrics for Prometheus](https://app.pluralsight.com/library/courses/instrumenting-applications-metrics-prometheus/table-of-contents) pluralsight course, mentions  producing `Info` metric and merging it with operational metric using orin in the __Recording application infomation using a custom metric__ slide.
  * [wget exit codes](https://gist.github.com/cosimo/5747881)
  * a similar [example](https://github.com/binhhq/prometheus-pushgateway-demo)
  * [documentation](https://prometheus.io/docs/prometheus/1.8/storage/) for sophisticated Prometheus data storage subsystem
  * [documentation](https://prometheus.io/docs/guides/node-exporter/) for Prometheus Node Exporter
  * [documentation](https://prometheus.io/docs/guides/multi-target-exporter/) on muti-target scraping scenario configuring Prometheus to scrape exporters using relabeling
  * [windows_exporter](https://github.com/prometheus-community/windows_exporter) - Prometheus exporter for Windows machines (community).


### Author


[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
