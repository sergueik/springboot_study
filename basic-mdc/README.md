### Info

Tnis directory contains a replica of [Zipkin](https://zipkin.io)-based [MDC](http://logback.qos.ch/manual/mdc.html)
[demo](https://github.com/TilinC/demo)  applications converted to maven. 


Note the Zipkin-provided `traceid` format is apparently different from the [w3c standard](https://www.w3.org/TR/trace-context/), 
the conversion to Elasic APM's [distributed open tracing](https://www.elastic.co/blog/distributed-tracing-opentracing-and-elastic-apm) is pending, also it creating  a bundle of several apps to illustrate inheritance and carryover of the `traceid`

 
### Usage
* build and launch
```sh
mvn spring-boot:run
java -jar target/mdc*.jar
```
* interact with application
```sh
curl -s http://localhost:8080/
```
this fill echo the message to  the console:
```text
traceId: 4124b7a0088b4b3b
```

the application console log will show that value of `traceid` in messages


```text
32481 [traceId=4124b7a0088b4b3b/spanId=4124b7a0088b4b3b] [http-nio-8080-exec-2] INFO  example.BusinessAppController - hello BusinessAppController
```
and additionally will log from `brave.Tracer` (formatted for readability):
```text
7187 [traceId=/spanId=] [http-nio-8080-exec-1] INFO  brave.Tracer - 
{
  "traceId": "4124b7a0088b4b3b",
  "id": "4124b7a0088b4b3b",
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
  * [article](https://www.baeldung.com/mdc-in-log4j-2-logback) and [sample project](https://github.com/eugenp/tutorials/tree/master/logging-modules/log-mdc) on improved Java Logging with Mapped Diagnostic Context (MDC)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
