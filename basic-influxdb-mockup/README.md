### Info

Pure Java [Line Protocol](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_tutorial/) parser to serve as [influxDB](https://docs.influxdata.com/influxdb/v1.8/) server [fake](https://martinfowler.com/bliki/TestDouble.html)

The Controller reads the payloads, and passes it to super simplified Parser utility which identifies the parts (but not saves then anywhere nor constrct the Measurement object)


### Usage
* test
```
mvn test
```
* run 

```sh
mvn spring-boot:run
```
* test with `curl`
```
curl -v -s -X POST -d "test,foo=bar value=42 1655244130852723" http://localhost:8085/write?db=example
```
this will respond with
```text
  POST /write?db=example HTTP/1.1
  Host: localhost:8085
  User-Agent: curl/7.74.0
  Accept: */*
  Content-Length: 38
  Content-Type: application/x-www-form-urlencoded
 
 [38 bytes data]
* upload completely sent off: 38 out of 38 bytes
* Mark bundle as not supporting multiuse
  HTTP/1.1 204
  result: measurement=test tag_set=foo=bar field_set=value=42 timestamp=1655244130852723
  Date: Fri, 17 Jun 2022 21:01:45 GMT
 
* Connection #0 to host localhost left intact
```
- imitating the genuine InfluxDB response plus the `result` headers

and application logs 

```text
2022-06-15 20:45:59.427  INFO 7896 --- [nio-8085-exec-3] example.controller.Controller            : body: db=example&test%2Cfoo=bar+value%3D42+1655244130852723
2022-06-15 20:45:59.437  INFO 7896 --- [nio-8085-exec-3] example.controller.Controller            : input: test,foo=bar value=42 1655244130852723
2022-06-15 20:45:59.451  INFO 7896 --- [nio-8085-exec-3] example.utils.Utils                      : input: test,foo=bar value=42 1655244130852723
2022-06-15 20:45:59.470  INFO 7896 --- [nio-8085-exec-3] example.utils.Utils: Found match.
2022-06-15 20:45:59.478  INFO 7896 --- [nio-8085-exec-3] example.controller.Controller            : result: measurement=testtag_set=foo=barfield_set=value=42timestamp=1655244130852723
```

the additonal endpoint `write2` was implemented to reproduce the processing of `/write` but return the loaded `Data Point` object as JSON in response body:
```sh
PAYLOAD="test,foo=bar,baz=bam value=42,data=10 1655244130852723" 
curl -s -X POST -d "${PAYLOAD}" "http://localhost:8085/write2?db=example&precision=s"| jq  '.'
```
which will  resspond with
```JSON
{
  "measurement": "test",
  "tags": {
    "foo": "bar",
    "baz": "bam"
  },
  "fields": {
    "data": "10",
    "value": "42"
  },
  "precision": "SECONDS",
  "time": 1655244130852723
}

```
The `ping` request is also implemented:
```sh
curl -v -s -X HEAD http://localhost:8085/ping
```
responds with
```text
  HEAD /ping HTTP/1.1
  Host: localhost:8085
  User-Agent: curl/7.74.0
  Accept: */*
 
* Mark bundle as not supporting multiuse
  HTTP/1.1 204
  X-Influxdb-version: OSS
  Date: Fri, 17 Jun 2022 21:03:36 GMT 
```
* testing with [Perl InfluxDB Client](https://metacpan.org/pod/InfluxDB::Client::Simple)

```sh
mvn spring-boot:run
```
```sh
cd ../basic-influxdb
perl -I . test-small.pl
```
this will respond with
```text
Reporting host: sergueik71
Write
Send Data
Send Data
Send Data
```
and app is loggng 
```text
2022-06-16 03:12:14.388  INFO 4748 --- [nio-8085-exec-4] example.controller.Controller            : body: db=example&testing%2Cappid=BAZ%2Cenv%3DUAT%2Chost%3Dsergueik71%2Coperation%3Dsend+value%3D42.0+1655341934
2022-06-16 03:12:14.390  INFO 4748 --- [nio-8085-exec-4] example.controller.Controller            : input: testing,appid=BAZ,env=UAT,host=sergueik71,operation=send value=42.0 1655341934
2022-06-16 03:12:14.390  INFO 4748 --- [nio-8085-exec-4] example.utils.Utils                      : input: testing,appid=BAZ,env=UAT,host=sergueik71,operation=send value=42.0 1655341934
2022-06-16 03:12:14.390  INFO 4748 --- [nio-8085-exec-4] example.utils.Utils                      : Found match.
2022-06-16 03:12:14.391  INFO 4748 --- [nio-8085-exec-4] example.controller.Controller            : result: measurement=testing
tag_set=operation=send
field_set=value=42.0
timestamp=1655341934

```
The posted data is not currently stored anywhere - the service is fake. The parser may evolve later to do the actual measurement intance extraction

if the `debug` flag is provided to `test-small.pl`, some data will be visible in `Raw content:`,  but overal the Perl module expects JSON response.

### Line Protocol

* the format is very elementary
```text
measurement|,tag_set| |field_set| |timestamp
```
* measurement and tag set are separated by a comma and no spaces

* key-value pairs are joined with an equals sign = and no spaces
```text
<field_key>=<field_value>
```
* separate multiple tag-value pairs are joined with a comma and no spaces
```text
<tag_key>=<tag_value>,<tag_key>=<tag_value>
```
* the string field values are always expected to be double quoted
* separate the field set and the optional timestamp with a whitespace (required by the protocol)
* the timestamp for data point in nanosecond-precision Unix time unless a differnt `precision` is provided 

### See Also

   * [intro to InfluxSB](https://tproger.ru/translations/influxdb-guide/) (in Russian)
   * [guide to Spring Boot REST API Error Handling](https://www.toptal.com/java/spring-boot-rest-api-error-handling)
   * [custom error message handling for REST API](https://www.baeldung.com/global-error-handler-in-a-spring-rest-api)
   * [error handling for REST with Spring](https://www.baeldung.com/exception-handling-for-rest-with-spring)
   * https://metacpan.org/pod/CGI#DEBUGGING
  
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
