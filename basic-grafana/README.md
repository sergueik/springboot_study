### Info

An __Grafana 5.0.4__  container based on [container-examples](https://github.com/container-examples/alpine-grafana) repository,
switched to available [alpine 3.9 Docker image with glibc](https://hub.docker.com/r/frolvlad/alpine-glibc/)
base image with only JSON Datasource plugins 

 * [simple-json](https://grafana.com/grafana/plugins/grafana-simple-json-datasource/) 
 * [simpod-json-datasource](https://grafana.com/grafana/plugins/simpod-json-datasource/)
 
installed

### Testing

```sh
IMAGE=basic-grafana
GRAFANA_VERSION=5.0.4
wget https://s3-us-west-2.amazonaws.com/grafana-releases/release/grafana-${GRAFANA_VERSION}.linux-x64.tar.gz .
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

```
### See Also

  https://github.com/grafana/grafana-docker/blob/master/Dockerfile
  https://grafana.com/docs/grafana/latest/http_api/data_source/
  https://github.com/cryostatio/jfr-datasource
  https://www.linkedin.com/pulse/implementing-grafana-simplejson-datasource-using-sparkjava-samant/



