### Info

a replica of [grafana rrd server](https://github.com/doublemarket/grafana-rrd-server) pinned to specific alpine version

### Usage

* compile and conteinerise app
```sh
RRD_SERVER=basic-rrd
docker build -f Dockerfile -t $RRD_SERVER .
```
* run in container
followed by
```sh
docker run --name $RRD_SERVER -v $(pwd)/sample/:/sample -p 9001:9000 -d $RRD_SERVER
```
 * check via curl call
```sh
curl -s http://localhost:9000/search | jq
```
or if like to limit the files to scan, with POST request
```sh
curl -s -X POST -H 'Content-Type: application/json' -d '{"target": "sample" }' http://localhost:9000/search
```
it will show only matching 

```go
func search(w http.ResponseWriter, r *http.Request) {
...
target := searchRequest.Target
for _, path := range searchCache.Get() {
  if strings.Contains(path, target) {
    result = append(result, path)
  }
```
files
```json
[
  "sample:ClientInfoAge",
  "sample:ClientJobsRunning",
  "sample:ReqMaxRun",
  "sample:ClientGlideIdle",
  "sample:ClientGlideRunning",
  "sample:ClientGlideTotal",
  "sample:StatusStageIn",
  "sample:StatusRunning",
  "sample:ClientJobsIdle",
  "sample:ReqIdle",
  "sample:StatusIdle",
  "sample:StatusPending",
  "sample:StatusHeld",
  "sample:StatusIdleOther",
  "sample:StatusStageOut",
  "sample:StatusWait"
]
```
if outputs an empty result
```json
[]
```
investigate (seeing it with Dockerized instances)
when configured correctly it shows "target" metric catalog e.g. for `sample` directory 
```sh
annotations.csv
percent-idle.rrd
percent-user.rrd
sample.rrd
```
embeded in the upstream project:

```json
[
  "percent-idle:value",
  "percent-user:value",
  "sample:ClientGlideRunning",
  "sample:ClientGlideTotal",
  "sample:StatusIdle",
  "sample:StatusWait",
  "sample:ClientGlideIdle",
  "sample:ClientInfoAge",
  "sample:ClientJobsRunning",
  "sample:ReqIdle",
  "sample:StatusIdleOther",
  "sample:ClientJobsIdle",
  "sample:StatusHeld",
  "sample:StatusRunning",
  "sample:ReqMaxRun",
  "sample:StatusPending",
  "sample:StatusStageIn",
  "sample:StatusStageOut"
]
```
proceed
* launch grafana container linked to the above:

```sh
GRAFANA=basic-grafana
docker build -f Dockerfile -t $GRAFANA .
docker container run --link $RRD_SERVER --name $GRAFANA -d -p 3000:3000 $GRAFANA
```

* cleaup
```sh
docker container stop $RRD_SERVER
docker container rm $RRD_SERVER
```


it should process https://grafana.com/grafana/plugins/grafana-simple-json-datasource/
### Installon the host

```sh
wget -O grafana-rrd-server_linux_amd64.gz https://github.com/doublemarket/grafana-rrd-server/releases/download/v0.0.5/grafana-rrd-server_linux_amd64.gz
gunzip  grafana-rrd-server_linux_amd64.gz
chmod +x grafana-rrd-server_linux_amd64
sudo mv grafana-rrd-server_linux_amd64 /usr/local/bin/grafana-rrd-server
```
then point  grafana docker image to host ip addess as usual

![Dashboard Example](https://github.com/sergueik/springboot_study/blob/master/basic-grafana-rrd-server/screenshots/capture-grafana.png)
* alternatively query the RRD data source directly:
```sh
curl -X POST http://localhost:9000/query -d '
{
  "timezone": "browser",
  "panelId": 2,
  "range": {
    "from": "2010-03-02T04:57:48.126Z",
    "to": "2010-03-02T05:42:32.733Z",
    "raw": {
      "from": "2010-03-02T04:57:48.126Z",
      "to": "2010-03-02T05:42:32.733Z"
    }
  },
  "rangeRaw": {
    "from": "2010-03-02T04:57:48.126Z",
    "to": "2010-03-02T05:42:32.733Z"
  },
  "interval": "2s",
  "intervalMs": 2000,
  "targets": [
    {
      "target": "sample:ClientJobsRunning",
      "refId": "A",
      "type": "timeserie"
    }
  ],
  "maxDataPoints": 928,
  "scopedVars": {
    "__interval": {
      "text": "2s",
      "value": "2s"
    },
    "__interval_ms": {
      "text": 2000,
      "value": 2000
    }
  }
}
'

```
it will respond with
```json
[
  {
    "target": "sample:ClientJobsRunning",
    "datapoints": [
      [
        164381.51527777777,
        1267502400000
      ],
      [
        144435.16694444444,
        1267506000000
      ]
    ]
  }
]
```
Note: even with incomplete query payload
```sh
[
  {
    "target": "sample:ClientJobsRunning",
    "datapoints": null
  }
]

```
the RRD Grafana Server will still respond  meaningfully:
```json
[
  {
    "target": "sample:ClientJobsRunning",
    "datapoints": null
  }
]
```

when no data is available for specified target, a null response will be returned. The query is the same as sent by the Grafana UI, with few details omitted

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
The required version of the shared library was found through system wide scan by root user
inside several overlayy directories of the dockerised container:
```sh
sudo -s
find / -iname 'librrd*' 2>/dev/null | tee /tmp/a.log
```
```sh
/var/lib/docker/overlay2/b1bc4e07c66d52d8b73db2c4404b0a7e40d7f8b5562ffd71e6e49d67c608c47c/diff/usr/lib/librrd.so.8
```
To verify extract the buld portion of the Dockerfile
build
```sh
RRD_SERVER_BUILD=xxx
docker build -t $RRD_SERVER_BUILD -f Dockerfile.build .
```
and investigate
```sh
docker run -it $RRD_SERVER_BUILD
```
to see
```sh
cd /build
ls -l lib/librrd.so.8
lrwxrwxrwx    1 root     root            15 Aug  2 22:15 lib/librrd.so.8 -> librrd.so.4.3.5
readlink lib/librrd.so.8
librrd.so.4.3.5
```

This issue is discussed - see soft link [suggestion](https://github.com/doublemarket/grafana-rrd-server/issues/44):
```sh
sudo -s
cd /usr/lib/x86_64-linux-gnu/
ln -fs librrd.so.4.3.5  librrd.so.8
```
* alternatively  can copy the  needed libs from `/usr/lib/x86_64-linux-gnu` to a non standard lib directory e.g. `lib` and provide the `LD_LIBRARY_PATH` setting:
```sh
mkdir lib
cp -R /usr/lib/x86_64-linux-gnu/librrd* ./lib
cd lib/
ln -fs librrd.so.4.3.5  librrd.so.8
export LD_LIBRARY_PATH=$(pwd)
cd ..
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
This is caused by not selecting the relevant date range
to find out what data is available use command
```sh
rrdtool dump sample/sample.rrd
```

### See Also

  * java time series data browser with JavaFx [app](https://github.com/binjr/binjr) backed by RRD
   * java port of rrdtool - [rrd4j](https://github.com/rrd4j/rrd4j)
   * RRDTool [tutorial](https://oss.oetiker.ch/rrdtool/tut/index.en.html)
   * [post](https://medium.com/@raghavendrasamant/simplejson-datasource-implementation-in-grafana-using-sparkjava-81e2274b1cfa) about java based simpleJSON backend
  * https://github.com/OpenNMS/jrrd - mixed Java / C wrapper
  * [RRDtool Tutorial - Part 1](https://www.youtube.com/watch?v=JaK-IctEyWs)
  * [RRDtool Tutorial - Part 2](https://www.youtube.com/watch?v=m_qeVVB2yzw)
  * https://github.com/sysmo-nms/rrdio
  * probably moves data the other direction https://github.com/nitinka/JMetrics