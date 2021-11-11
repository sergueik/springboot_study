### Info


this directory contains a replica of a 
[simple Spring Boot application](https://github.com/arun-gupta/spring-boot-prometheus) that computes and exposes Proemtheus metrics
relying on springboot actuator to handle the endpoint that returns
prometheus metrics. Currently generates but does not export the appication specific metrics

### Usage

 * run application:

```sh
mvn spring-boot:run
```
* index page
```curl
http://localhost:8080/
```
* access the application
```sh
curl http://localhost:8080/hello
```
* access system metrics
```sh
curl http://localhost:8080/actuator/prometheus
```
will return
```text
# HELP logback_events_total Number of error level events that made it to the logs
# TYPE logback_events_total counter
logback_events_total{level="warn",} 0.0
logback_events_total{level="debug",} 0.0
logback_events_total{level="error",} 0.0
logback_events_total{level="trace",} 0.0
logback_events_total{level="info",} 8.0
...
# HELP http_server_requests_seconds  
# TYPE http_server_requests_seconds summary
http_server_requests_seconds_count{exception="None",method="GET",outcome="SUCCESS",status="200",uri="/metrics",} 1.0
http_server_requests_seconds_sum{exception="None",method="GET",outcome="SUCCESS",status="200",uri="/metrics",} 0.167960676
http_server_requests_seconds_count{exception="None",method="GET",outcome="SUCCESS",status="200",uri="/actuator/prometheus",} 1.0
http_server_requests_seconds_sum{exception="None",method="GET",outcome="SUCCESS",status="200",uri="/actuator/prometheus",} 
...
```

* access appliction defined metrics
```sh
curl http://localhost:8080/metrics
```
will return
```text
# HELP requests_total Total number of requests.
# TYPE requests_total counter
requests_total 0.0
# HELP requests_latency_seconds Request latency in seconds.
# TYPE requests_latency_seconds histogram
requests_latency_seconds_bucket{le="0.005",} 0.0
requests_latency_seconds_bucket{le="0.01",} 0.0
...
requests_latency_seconds_bucket{le="+Inf",} 0.0
requests_latency_seconds_count 0.0
requests_latency_seconds_sum 0.0
```

### See Also 
 * [minimal prometheus conter example](https://github.com/njanor/spring-boot-prometheus-demo) - uses older Springboot and cannot compile
 *  [stepby step springboot monitoring dashboard example](https://github.com/rishant/springboot-monitoring-dashboard-example)
 * old [MVC example](https://github.com/ConSol/springboot-monitoring-example)
with a static page links to generated metrics, the application handles the `/metrics` route on its own. 

