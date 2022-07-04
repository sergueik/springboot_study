### Info

replica of [prometheus/pushgateway](https://github.com/prometheus/pushgateway) source to add the `timestamps` and expiration(to have a shorter name) option similar to existing in [prometheus/influxdb_exporter](https://github.com/prometheus/influxdb_exporter) to allog ephemeral jobs to enforce the timestamp of data points
### Note

* using commit hash `b7e0167e9574f4f88404dde9653ee1d3c940f2eb` to prevent upgrading to `golang` __17__.

* the build process is extremely time consuming

### Usage

* compile go program and copy locally

```sh
export IMAGE=basic-builder
docker image rm -f $IMAGE
docker build -t $IMAGE -f Dockerfile.builder .
```
```sh
export IMAGE=basic-go-build
docker image rm -f $IMAGE
docker build -t $IMAGE -f Dockerfile.build .
export NAME=basic-go-build
docker container rm $NAME
docker run -d --name=$NAME $IMAGE
docker cp $NAME:/build/example .
```
build run image
```sh
IMAGE=basic-go-run
docker build -t $IMAGE -f Dockerfile.run .
docker container rm -f $IMAGE
```
```sh
docker run --name $IMAGE -v $(pwd)/sample/:/sample -p 9001:9000 -it $IMAGE
```
NOTE: the build commands are borrowed from `rrdserver` [project](https://github.com/sergueik/springboot_study/tree/master/basic-go-mysql) and will need review


### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
