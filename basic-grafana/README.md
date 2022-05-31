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
GRAFANA_VERSION=7.3.4
wget https://dl.grafana.com/oss/release/grafana-${GRAFANA_VERSION}.linux-amd64.tar.gz .
```
NOTE, the download link can be pasted into browser address. To find the link, first browse the download page, e.g. for version __7.3.4__ it is [https://grafana.com/grafana/download/7.3.4?edition=oss](https://grafana.com/grafana/download/7.3.4?edition=oss). For change logs see [https://github.com/grafana/grafana/releases?after=v7.3.7](https://github.com/grafana/grafana/releases?after=v7.3.7)

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
* or when there is a server to link
```sh
SERVER=basic-go-run
docker container run --name $IMAGE --link $SERVER -d -p 3000:3000 $IMAGE
```
eventually it will show
```text
lvl=info msg="HTTP Server Listen" logger=http.server address=[::]:3000 protocol=http subUrl= socket=
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
this will show something like

```text
3: docker0: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc noqueue state UP group default
    link/ether 02:42:4d:f1:51:2e brd ff:ff:ff:ff:ff:ff
    inet 172.17.0.1/16 brd 172.17.255.255 scope global docker0
       valid_lft forever preferred_lft forever
```
* alternatively list the networks:
```sh
docker network list
```
```text
9af6b8700c5e   bridge                                    bridge    local
38e910f2eda5   host                                      host      local
90b8867ef029   none                                      null      local
```
unless specified otherwise the container will be on `bridge` network. Inspect that network gateway:
```sh
docker inspect bridge
```
and browse the output or provide a `format` argument:
```sh
docker inspect bridge --format '{{ json .IPAM.Config }}'
```
```JSON
[
  {
    "Subnet": "172.17.0.0/16",
    "Gateway": "172.17.0.1"
  }
]
```
alternatively can filter the output with `jq`:
```sh
docker inspect bridge | jq '.[].IPAM.Config'
```
then connect to the container and ping the `bridge` network gateway:
```sh
ping -c 1 172.17.0.1
```

```text
PING 172.17.0.1 (172.17.0.1): 56 data bytes
64 bytes from 172.17.0.1: seq=0 ttl=64 time=0.158 ms

--- 172.17.0.1 ping statistics ---
1 packets transmitted, 1 packets received, 0% packet loss
round-trip min/avg/max = 0.158/0.158/0.158 ms

```
of test listening ports:
```sh
nc -z 172.17.0.1 22
echo $?
```
```text
0
```
*  confirm that the data source is visible from grafana `$IMAGE` container:
```sh
docker exec -it $IMAGE curl -XGET http://172.17.0.1:5000/
```
will reply
```sh
OK
```
alternatively can try use `host.docker.internal` predefined hostname:
```sh
docker exec -it $IMAGE  curl -XGET http://host.docker.internal:5000/
```
it is [not guaranteed to work](https://github.com/docker/for-mac/issues/2965)

```text
nslookup: can't resolve 'host.docker.internal': Name does not resolve
```
in fact it may fail with older versions of Ubuntu - too recent versions of Docker required to install to have this feature working.

alternatively one can extract the external ip address of the host 

```sh
IP=$(ip -4 route list match 0/0 | awk '{print $3}')
```

* add datasource in the browser and configure __Simple JSON__ datasource to use that url `http://172.17.0.1:5000`
* alternatively if `docker-compose.yaml` is used add the setting:
```yaml
extra_hosts:
  - "developer:$( hostname -I | awk '{print $1}')"
```
- replace with actual ip address of the developer host
there and use `http://developer:5000` in grafana. See also [the documentation](https://docs.docker.com/compose/compose-file/compose-file-v3/)

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
  * [populating Spring @Value during Unit Test](https://newbedev.com/populating-spring-value-during-unit-test)
  * another [snippet](https://gist.github.com/danlangford/3418696) od dealing with @Value annotations in test runner

   * [discussion](https://stackoverflow.com/questions/48546124/what-is-linux-equivalent-of-host-docker-internal/61001152) about the linux equivalent of "host.docker.internal"

### Author

[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

