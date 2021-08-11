### Info

An __7.5.0__ container based on [container-examples](https://github.com/container-examples/alpine-grafana) repository,
switched to available [alpine 3.9 Docker image with glibc](https://hub.docker.com/r/frolvlad/alpine-glibc/)
base image with only JSON Datasource plugins 

 * [simple-json](https://grafana.com/grafana/plugins/grafana-simple-json-datasource/) 
 * [simpod-json-datasource](https://grafana.com/grafana/plugins/simpod-json-datasource/)
 
installed and a fake datasource with a trivial timeseries from [Jonnymcc/grafana-simplejson-datasource-example](https://github.com/Jonnymcc/grafana-simplejson-datasource-example/)
and the springboot application with similar functionality

### Testing
* optionally pre-download grafana package (it seems to be ignored by Docker `ADD` instruction)
```sh
GRAFANA_VERSION=7.5.0
wget https://dl.grafana.com/oss/release/grafana-${GRAFANA_VERSION}.linux-amd64.tar.gz .
```
* build the image
```sh
IMAGE=basic-grafana
docker build -f Dockerfile -t $IMAGE .
```
followed by
```sh
docker container run --name $IMAGE -d -p 3000:3000 $IMAGE
docker logs $IMAGE
```

* run the `index.py` as flask application in tbe foreground on a separate terminal
```sh
python index.py
```
alternatively run the supplied java appp:
```sh
mvn spring-boot:run
```
*  find the host ip address of `docker0` device to [use docker host from inside the container](https://stackoverflow.com/questions/31324981/how-to-access-host-port-from-docker-container):
```sh
ip address show docker0
```
*  confirm that the data source is OK:
```sh
docker exec -it $IMAGE  curl -XGET http://172.17.0.1:5000/
```
```sh
OK
```
* add datasource in the browser and configure __Simple JSON__ datasource to use that url `http://172.17.0.1:5000`

![datasource](https://github.com/sergueik/springboot_study/blob/master/basic-grafana/screenshots/capture_datasource.png)

After clicking __Save & Test__ it will respond with __Data source is working__

* add dashboard panel with this data source. 

![dashboard](https://github.com/sergueik/springboot_study/blob/master/basic-grafana/screenshots/capture_dashbpoard.png)

Of course it will be fake: no matter the time interval it will alwats diplay a straight line
the query details will be logged in the flask application console (pretty-printed):
```sh
172.17.0.2 - - [22/Jun/2021 02:45:25] "POST /query HTTP/1.1" 200 -
/query:
{
  "startTime": 1624322725047,
  "rangeRaw": {
    "to": "now",
    "from": "now-6h"
  },
  "app": "dashboard",
  "interval": "20s",
  "scopedVars": {
    "__interval": {
      "text": "20s",
      "value": "20s"
    },
    "__interval_ms": {
      "text": "20000",
      "value": 20000
    }
  },
  "range": {
    "to": "2021-06-22T00:45:25.047Z",
    "from": "2021-06-21T18:45:25.046Z",
    "raw": {
      "to": "now",
      "from": "now-6h"
    }
  },
  "timeInfo": "",
  "requestId": "Q101",
  "panelId": 23763571993,
  "dashboardId": null,
  "timezone": "browser",
  "adhocFilters": [],
  "targets": [
    {
      "type": "timeserie",
      "target": "my_series",
      "refId": "A"
    }
  ],
  "maxDataPoints": 916,
  "intervalMs": 20000
}
```
### Troublshooting

```sh
docker run --entrypoint sh -p 3000:3000 -it $IMAGE
```
### Cleanup

```sh
docker stop $IMAGE
docker container prune -f
docker image prune -f
docker image rm $IMAGE
```
### DataSource

to support Grafana's simpleJson plugin, the back-end WebAPI implements 4 routes:
  *  `/`: return HTTP 200, used for healthcheck
  *  `/search`: return all optional indicators
  *  `/query`: return the time series points for specific indicator
  *  `/annotations`: return annotations

### Strongly-typed Implementation
```sh
mvn test
```
```sh
mvn -Dmaven.test.skip=true spring-boot:run
```

```sh
curl -s -X POST  http://localhost:5000/search2
```
```json
{
  "timestamp": 1628720447913,
  "status": 415,
  "error": "Unsupported Media Type",
  "exception": "org.springframework.web.HttpMediaTypeNotSupportedException",
  "message": "Content type 'null' not supported",
  "path": "/search2"
}
```

```sh
curl -s -X POST  -H "Content-Type: application/json" http://localhost:5000/search2 2>& 1 | /c/tools/jq-win64.exe  '.'

```
```json
{
  "timestamp": 1628719457372,
  "status": 400,
  "error": "Bad Request",
  "exception": "org.springframework.http.converter.HttpMessageNotReadableException",
  "message": "Required request body is missing: public org.springframework.http.ResponseEntity<java.util.List<example.component.SearchResponseRow>> example.controller.DataSoureController.postSearch2Request(example.component.SearchRequest)",
  "path": "/search2"
}

```
```sh
 curl -v -s -X POST  -H "Content-Type: application/json" -d '{"target":"dummy"}' http://localhost:5000/search2 2>& 1

```
```text
[{"text":"text data","value":"value data"}]
* Uses proxy env variable no_proxy == '192.168.99.100'
*   Trying 127.0.0.1:5000...
* Connected to localhost (127.0.0.1) port 5000 (#0)
> POST /search2 HTTP/1.1
> Host: localhost:5000
> User-Agent: curl/7.74.0
> Accept: */*
> Content-Type: application/json
> Content-Length: 18
>
} [18 bytes data]
* upload completely sent off: 18 out of 18 bytes
* Mark bundle as not supporting multiuse
< HTTP/1.1 200
< Access-Control-Allow-Headers: accept, content-type
< Access-Control-Allow-Methods: POST
< Access-Control-Allow-Origin: *
< Content-Type: application/json
< Transfer-Encoding: chunked
< Date: Wed, 11 Aug 2021 22:15:57 GMT
<
{ [49 bytes data]
* Connection #0 to host localhost left intact
```
### See Also

  * https://github.com/grafana/grafana-docker/blob/master/Dockerfile
  * https://grafana.com/docs/grafana/latest/http_api/data_source/
  * https://github.com/cryostatio/jfr-datasource
  * https://www.linkedin.com/pulse/implementing-grafana-simplejson-datasource-using-sparkjava-samant/
  * https://www.programmersought.com/article/36167425429/
  * https://www.programcreek.com/java-api-examples/?api=org.jfree.data.time.TimeSeries
  * https://github.com/devcon5io/grafana-json-datasource
  * https://docs.docker.com/compose/networking/
  * https://github.com/signaflo/java-timeseries
  * https://github.com/chrandgull/granfana-rest-demo
  * https://github.com/TKnudsen/timeSeries
  * https://github.com/tschm/ts-timeseries
  * https://github.com/IBMStreams/samples
  * https://oznetnerd.com/2018/04/17/writing-a-grafana-backend-using-the-simple-json-datasource-flask/
  * basic implementation of [SimpleJSON REST server](https://github.com/IsmetKh/grafana-simplejson-datasource) - has ASP.Net dependencies , can be used to prototype `/query`, `/search`,`/annotations`, `/tag-keys`, `tag-alues` payloads then using some prototype ASP.Net clean [REST famework](https://github.com/sachabarber/REST/tree/master/RESTServer/RESTServer) also discussed in [codeproject article](https://www.codeproject.com/Articles/826383/REST-A-Simple-REST-framework)

### Author

[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

