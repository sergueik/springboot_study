### Info

a replica of [grafana rrd server](https://github.com/doublemarket/grafana-rrd-server) pinned to specific alpine version

### Usage

* compile and conteinerise app
```sh
IMAGE=basic-example
docker build -f Dockerfile -t $IMAGE .
```
* run in container
followed by
```sh
docker run --name $IMAGE -v $(pwd)/sample/:/sample -p 9000:9000 -d $IMAGE
```
* cleaup
```sh
docker container stop $IMAGE
docker container rm $IMAGE
```


it should process https://grafana.com/grafana/plugins/grafana-simple-json-datasource/
### Installon the host

```sh
wget -O grafana-rrd-server_linux_amd64.gz https://github.com/doublemarket/grafana-rrd-server/releases/download/v0.0.5/grafana-rrd-server_linux_amd64.gz
gunzip  grafana-rrd-server_linux_amd64.gz
chmod +x grafana-rrd-server_linux_amd64
sudo mv grafana-rrd-server_linux_amd64 /usr/local/bin/grafana-rrd-server
```
### Challenges

* intermittent error when building
if swapped the `golang:1.16-alpine` with other alpine golang base image (a few tried):

```txt
rrdserver.go:17:2: cannot find package "github.com/gocarina/gocsv" in any of:
  /usr/lib/go/src/github.com/gocarina/gocsv (from $GOROOT)
  /go/src/github.com/gocarina/gocsv (from $GOPATH)
rrdserver.go:18:2: cannot find package "github.com/mattn/go-zglob" in any of:
  /usr/lib/go/src/github.com/mattn/go-zglob (from $GOROOT)
  /go/src/github.com/mattn/go-zglob (from $GOPATH)
rrdserver.go:19:2: cannot find package "github.com/ziutek/rrd" in any of:
  /usr/lib/go/src/github.com/ziutek/rrd (from $GOROOT)
  /go/src/github.com/ziutek/rrd (from $GOPATH)
```

* runtime
```sh
grafana-rrd-server -h
grafana-rrd-server: error while loading shared libraries: librrd.so.8: cannot open shared object file: No such file or directory
```
see soft link [suggestion](https://github.com/doublemarket/grafana-rrd-server/issues/44):
```sh
sudo -s
cd /usr/lib/x86_64-linux-gnu/
ln -fs librrd.so.4.3.5  librrd.so.8
```

* dockerized
when processing the query
```sh
curl http://localhost:9000/search
```
seing the error
```sh
ERROR: Cannot decode the request
EOF
```
and an empty response
```json
[]
```
no problem when running application standlone on the host
### See Also

  * java time series data browser with JavaFx [app](https://github.com/binjr/binjr) backed by RRD
   * java port of rrdtool - [rrd4j](https://github.com/rrd4j/rrd4j)


