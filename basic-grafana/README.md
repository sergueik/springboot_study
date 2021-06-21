### Info

An __Grafana 5.0.4__  container based on [container-examples](https://github.com/container-examples/alpine-grafana) repository,
switched to available [alpine 3.9 Docker image with glibc](https://hub.docker.com/r/frolvlad/alpine-glibc/)
base image with only JSON Datasource plugins 

 * [simple-json](https://grafana.com/grafana/plugins/grafana-simple-json-datasource/) 
 * [simpod-json-datasource](https://grafana.com/grafana/plugins/simpod-json-datasource/)
 
installed. 

Note: the release __7.3.0__ is possible to installl, but the JSON datasources do not show in the UI.


### Testing
* optionally pre-download grafana package (it seems to be ignored by Docker `ADD` instaruction)
```sh
GRAFANA_VERSION=7.3.0
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
Open the dashboard in the browser and add datasources (still need to build sample backend first)

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
  * https://github.com/TKnudsen/timeSeries
  * https://github.com/tschm/ts-timeseries

### Author

[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

