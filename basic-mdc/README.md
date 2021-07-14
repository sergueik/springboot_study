### Info

Tnis directory contains a replica of [Zipkin](https://zipkin.io)-based [MDC](http://logback.qos.ch/manual/mdc.html)
[demo](https://github.com/TilinC/demo)  applications converted to maven. 
Note the Zipkin-provided `traceid` format is apparently different from the [w3c standard](https://www.w3.org/TR/trace-context/), 
the conversion to Elasic APM's [distributed open tracing](https://www.elastic.co/blog/distributed-tracing-opentracing-and-elastic-apm) is pending, also it creating  a bundle of several apps to illustrate inheritance and carryover of the traceid

 
### Usage
* build and launch
```sh
mvn spring-boot:run
java -jar target/mdc*.jar
```
* interact with application
```sh
curl http://localhost:8080/
```
this fill log
```text
6928 [traceId=/spanId=] [http-nio-8080-exec-1] INFO  o.s.web.servlet.DispatcherServlet - FrameworkServlet 'dispatcherServlet': initialization completed in 57 ms
7057 [traceId=ccb28fb43116ad6e/spanId=ccb28fb43116ad6e] [http-nio-8080-exec-1] INFO  example.BusinessAppController - hello BusinessAppController
```
and additionally will log from `brave.Tracer` (formatted for readability):
```text
7187 [traceId=/spanId=] [http-nio-8080-exec-1] INFO  brave.Tracer - 
{
  "traceId": "ccb28fb43116ad6e",
  "id": "ccb28fb43116ad6e",
  "name": "get",
  "timestamp": 1626221024879705,
  "duration": 92966,
  "annotations": [
    {
      "timestamp": 1626221024879705,
      "value": "sr",
      "endpoint": {
        "serviceName": "business-app",
        "ipv4": "192.168.51.1"
      }
    },
    {
      "timestamp": 1626221024972671,
      "value": "ss",
      "endpoint": {
        "serviceName": "business-app",
        "ipv4": "192.168.51.1"
      }
    }
  ],
  "binaryAnnotations": [
    {
      "key": "ca",
      "value": true,
      "endpoint": {
        "serviceName": "",
        "ipv4": "127.0.0.1",
        "port": 55292
      }
    },
    {
      "key": "http.path",
      "value": "/",
      "endpoint": {
        "serviceName": "business-app",
        "ipv4": "192.168.51.1"
      }
    }
  ]
}


```
### See Also
  * https://dzone.com/articles/end-to-end-distributed-logging-traceability-with-c
