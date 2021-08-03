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
docker run --name $RRD_SERVER -v $(pwd)/sample/:/sample -p 9000:9000 -d $RRD_SERVER
```
 * check via curl call
```sh
 curl -s http://192.168.0.29:9000/search | jq
```
if outouts an empty  result
```json
[]
```
investigate (seeing it with Dockerizd instances)
otherwise if shows expected data catalog e.g. for sample.rrd embeded in the upstream project, if

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
see soft link [suggestion](https://github.com/doublemarket/grafana-rrd-server/issues/44):
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
