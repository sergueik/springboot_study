### Info

This directory contains a replica of the Prometheus [application runtime metric exporter](https://github.com/nlighten/tomcat_exporter) for Apache Tomcat, deployed in the same container with a legacy app which in this example is represented by an ultra basic static html file put into tomcat docker container

### Testing

* compile the servlet
```sh
pushd exporter; mvn clean package; popd
```
The servlet `pom.xml` was refactored from the upstream version to not use the parent pom and the `copy-dependencies` goal defined to have all needed jar under `target/lib` The exception 
```java
java.lang.IllegalArgumentException: Failed to register Collector of type ClassLoadingExports: The Collector exposes the same name multiple times: jvm_classes_loaded
```
in the servlet initialization code was hidden in a `try`...`catch` block
* compile the dummy app
```sh
pushd app; mvn clean package ; popd
```
* build the image
```sh
IMAGE=basic-tomcat-exporter
docker build -t $IMAGE -f Dockerfile .
NAME=$IMAGE	
docker run --name $NAME -d -p 8080:8080 $IMAGE
```
NOTE: the `tomcat_exporter_servlet.war` is to be deployed into the web apps directory, `${CATALINA_HOME}/webapps/` as `metrics.war`. The dependency jars `simpleclient.jar`, `simpleclient_common.jar`,`simpleclient_hotspot.jar`, `simpleclient_servlet_common.jar`, and `tomcat_exporter_client.jar` has to be put into `${CATALINA_HOME}/lib`

* confirm the app deployed and handles the request:
```sh
curl -sI http://localhost:8080/dummy/index.html
```
```text
HTTP/1.1 200
Accept-Ranges: bytes
ETag: W/"75-1675005398000"
Last-Modified: Sun, 29 Jan 2023 15:16:38 GMT
Content-Type: text/html
Content-Length: 75
Date: Sun, 29 Jan 2023 21:24:59 GMT
```
* ispect the metrics
```sh
curl -s http://192.168.0.92:8080/metrics/
```
will print
```text
# HELP jvm_threads_current Current thread count of a JVM
# TYPE jvm_threads_current gauge
jvm_threads_current 39.0
# HELP jvm_threads_daemon Daemon thread count of a JVM
# TYPE jvm_threads_daemon gauge
jvm_threads_daemon 38.0
# HELP jvm_threads_peak Peak thread count of a JVM
# TYPE jvm_threads_peak gauge
jvm_threads_peak 39.0
# HELP jvm_threads_started_total Started thread count of a JVM
# TYPE jvm_threads_started_total counter
jvm_threads_started_total 39.0
# HELP jvm_threads_deadlocked Cycles of JVM-threads that are in deadlock waiting to acquire object monitors or ownable synchronizers
# TYPE jvm_threads_deadlocked gauge
jvm_threads_deadlocked 0.0
# HELP jvm_threads_deadlocked_monitor Cycles of JVM-threads that are in deadlock waiting to acquire object monitors
# TYPE jvm_threads_deadlocked_monitor gauge
jvm_threads_deadlocked_monitor 0.0
# HELP jvm_threads_state Current count of threads by state
# TYPE jvm_threads_state gauge
jvm_threads_state{state="BLOCKED",} 0.0
jvm_threads_state{state="WAITING",} 21.0
jvm_threads_state{state="TIMED_WAITING",} 7.0
jvm_threads_state{state="TERMINATED",} 0.0
jvm_threads_state{state="NEW",} 0.0
jvm_threads_state{state="RUNNABLE",} 11.0
...
```
200+ metrics

### Cleanup
```sh
docker container rm -f $NAME
```
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)


