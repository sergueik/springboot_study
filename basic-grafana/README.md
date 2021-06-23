### Info

An __7.5.0__ container based on [container-examples](https://github.com/container-examples/alpine-grafana) repository,
switched to available [alpine 3.9 Docker image with glibc](https://hub.docker.com/r/frolvlad/alpine-glibc/)
base image with only JSON Datasource plugins 

 * [simple-json](https://grafana.com/grafana/plugins/grafana-simple-json-datasource/) 
 * [simpod-json-datasource](https://grafana.com/grafana/plugins/simpod-json-datasource/)
 
installed and a fake datarource with a trivial timeseries from [Jonnymcc/grafana-simplejson-datasource-example](https://github.com/Jonnymcc/grafana-simplejson-datasource-example/) and the springboot
application with similar functionality

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
docker run --name $IMAGE -d -p 3000:3000 $IMAGE
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
the query detauls will be logged in the flask application console:
```sh
172.17.0.2 - - [22/Jun/2021 02:45:25] "POST /query HTTP/1.1" 200 -
/query:
"{u'startTime': 1624322725047, u'rangeRaw': {u'to': u'now', u'from': u'now-6h'}, u'app': u'dashboard', u'interval': u'20s', u'scopedVars': {u'__interval': {u'text': u'20s', u'value': u'20s'}, u'__interval_ms': {u'text': u'20000', u'value': 20000}}, u'range': {u'to': u'2021-06-22T00:45:25.047Z', u'from': u'2021-06-21T18:45:25.046Z', u'raw': {u'to': u'now', u'from': u'now-6h'}}, u'timeInfo': u'', u'requestId': u'Q101', u'panelId': 23763571993, u'dashboardId': None, u'timezone': u'browser', u'adhocFilters': [], u'targets': [{u'type': u'timeserie', u'target': u'my_series', u'refId': u'A'}], u'maxDataPoints': 916, u'intervalMs': 20000}"
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

### Author

[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

