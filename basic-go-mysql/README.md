###  Info

Combination of two docker containers to practice the examples from [golang MySQL Tutorial](https://tutorialedge.net/golang/golang-mysql-tutorial/)

and subject [Wiki](https://github.com/go-sql-driver/mysql/wiki/Example)
with [grafana rrd server](https://github.com/doublemarket/grafana-rrd-server)

Changing the code loading cache for later accessing the data in
[RRDTool files](https://oss.oetiker.ch/rrdtool/) and implement
SimpleJSON grafana data sources over `/search`, `query`, `annotations` [protocol](https://grafana.com/grafana/plugins/grafana-simple-json-datasource/)

One advantage the database provides is using __index__ one can update and retriee information one file at a time, not loading or deleting the hash fully on `Get(target)` or`Update()`.

![filesystem](https://github.com/sergueik/springboot_study/blob/master/basic-go-mysql/screenshots/files_capture.jpg)

![database table](https://github.com/sergueik/springboot_study/blob/master/basic-go-mysql/screenshots/table_capture.jpg)

The table schema could easily incorporate additional fields for sophisticated Update policies if necessary
```SQL
CREATE TABLE `cache_table` (
  `id`        mediumint    NOT NULL AUTO_INCREMENT,
  `ins_date`  datetime     NOT NULL,
  `fname`     varchar(255) NOT NULL,
  `ds`        varchar(255) NOT NULL,
  `expose`    varchar(255) DEFAULT NULL,
  `comment`   varchar(255) DEFAULT NULL,
  INDEX(`FNAME`),
  PRIMARY KEY (`id`)
);
```
The column `expose` is serving the needs of sharng the RRD data of different kilds and return lists of files selected by
```sql
expose LIKE ?```
query template
where the value of the `expose` string determined from the request header (more about it below).

### Usage
*  have mysql container up
```sh
docker pull mysql:8.0.18
```
and run it with environmenti variables matching the hard-coded values in `example.go`:
```sh
docker run -v $(pwd):/tmp/app -p 3306:3306 --name mysql-server -e MYSQL_ROOT_PASSWORD=password -e MYSQL_USER=java -e MYSQL_DATABASE=test -e MYSQL_PASSWORD=password -d mysql:8.0.18
```
- published default port `3306` locally, but will not be using it directly
The required enviroment entries `MYSQL_ROOT_PASSWORD`, `MYSQL_USER`,`MYSQL_DATABASE`, `MYSQL_PASSWORD` are described in Mysql docker image.

Alternatively, if the container is already present but was stopped
```sh
docker container start mysql-server
```
NOTE: The server Docker instance will take quite some time to launch.
One can safely start building and runing golang app container while database initializes itself.
Eventually one
observes the successful start log message in `mysql-server` container:
```sh
docker logs mysql-server
```
```text
[Server] X Plugin ready for connections. Socket: '/var/run/mysqld/mysqlx.sock' bind-address: '::' port: 33060
```

*  compile go program and copy locally

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
docker build -t $IMAGE -f Dockerfile.run  .
docker container rm -f $IMAGE
```
* verify can connect locally on `mysql-server`:
```sh
docker exec -it mysql-server mysql -P 3306 -h localhost -u java -ppassword -e "set @var = '1'; select @var;"
```
```sh
mysql: [Warning] Using a password on the command line interface can be insecure.
+------+
| @var |
+------+
| 1    |
+------+
```

* Initialize DB
* NOTE: after any schema change need to relaunch the container because of volume (the first run takes some setup time)
```sh
docker container rm -f mysql-server
docker run -v $(pwd):/tmp/app -p 3306:3306 --name mysql-server -e MYSQL_ROOT_PASSWORD=password -e MYSQL_USER=java -e MYSQL_DATABASE=test -e MYSQL_PASSWORD=password -d mysql:8.0.18
docker logs mysql-server

```
```sh
docker exec -it mysql-server mysql -P 3306 -h localhost -u java -ppassword -e "source /tmp/app/mysql-init.sql"
```

* build cache
```sh
ln -fs ../basic-grafana-rrd-server/sample sample

docker run --link mysql-server --name $IMAGE -v $(pwd)/sample/:/sample -p 9001:9000 -i $IMAGE \
-u java -v password -w test -x mysql-server -y 3306 \
-update -verbose
```

this will log to console
```sh
Updating search cache.
Connected to database.
Delete from database:"percent-idle"
Inserted into database:"percent-idle:value"
Delete from database:"percent-user"
Inserted into database:"percent-user:value"
Delete from database:"sample"
Inserted into database:"sample:ClientInfoAge"
Inserted into database:"sample:StatusHeld"
...
Closed database connection.
Finished updating search cache.
```
if the file in the `data` directory is matching filename mask but is not a valid rrd, it is skipped, exception logged:
```sh
ERROR: Cannot retrieve information from  sample/web/badlink.rrd
opening 'sample/web/badlink.rrd': No such file or directory
```
followed by checks
```sh
docker exec -it mysql-server mysql -P 3306 -h localhost -u java -ppassword -e  "use test; show tables;";
```
```text
+----------------+
| Tables_in_test |
+----------------+
| cache_table    |
+----------------+
```
and
```sh
2>/dev/null docker exec -it mysql-server mysql -P 3306 -h localhost -u java -ppassword -e "use test; SELECT * FROM cache_table LIMIT 10";
```
```text
+----+---------------------+------------+--------------------+--------+---------+
| id | ins_date            | fname      | ds                 | expose | comment |
+----+---------------------+------------+--------------------+--------+---------+
|  1 | 2021-09-01 23:43:19 | fname-1    | ds-1               | NULL   | NULL    |
|  2 | 2021-09-01 23:43:19 | fname-1    | ds-2               | NULL   | NULL    |
|  3 | 2021-09-01 23:43:19 | fname-1    | ds-3               | NULL   | NULL    |
|  4 | 2021-09-01 23:43:19 | fname-2    | ds-4               | NULL   | NULL    |
|  5 | 2021-09-01 23:43:19 | fname-2    | ds-5               | NULL   | NULL    |
|  6 | 2021-09-01 23:43:19 | fname-3    | ds-5               | NULL   | NULL    |
|  7 | 2021-09-01 23:48:31 | app:sample | ClientGlideRunning | NULL   | NULL    |
|  8 | 2021-09-01 23:48:31 | app:sample | ClientJobsRunning  | NULL   | NULL    |
|  9 | 2021-09-01 23:48:31 | app:sample | ReqIdle            | NULL   | NULL    |
| 10 | 2021-09-01 23:48:31 | app:sample | StatusHeld         | NULL   | NULL    |
+----+---------------------+------------+--------------------+--------+---------+
```
Mark all rows with ds containing `Client` set `expose` to `data`:

```sh
2>/dev/null docker exec -it mysql-server mysql -P 3306 -h localhost -u java -ppassword -e "use test; UPDATE cache_table SET expose = 'data' WHERE ds LIKE '%Client%'";
```
and rows with ds containing `Status` set `expose` as `other`
```sh
2>/dev/null docker exec -it mysql-server mysql -P 3306 -h localhost -u java -ppassword -e "use test; UPDATE cache_table SET expose = 'other' WHERE ds LIKE '%status%'";
```
* start server
```sh
docker container rm -f $IMAGE
docker run --link mysql-server --name $IMAGE -v $(pwd)/sample/:/sample -p 9001:9000 -d $IMAGE  -u java -v password -w test -x mysql-server -y 3306 -verbose
docker logs $IMAGE
```
this will start web server
* try search
```sh
curl -s http://localhost:9001/search
```
this will be processing in the same way as a POST request with a `target` parameter by the latest revision.

* without servicing `GET` request with `/search` the grafana list box is empty. It is likely not even rendered by some releases (to be confirmed)
![broken listbox](https://github.com/sergueik/springboot_study/blob/master/basic-go-mysql/screenshots/broken_listbox_capture.jpg)

*  the free hand entry is still possible in which case when the parameters are correct the data appets shown

![freehand entry](https://github.com/sergueik/springboot_study/blob/master/basic-go-mysql/screenshots/freehand_entry_capture.png)

* after servicing  the `GET` request with `/search` the list box is populated
![fixed listbox](https://github.com/sergueik/springboot_study/blob/master/basic-go-mysql/screenshots/fixed_listbox_capture.png)
```sh
curl -s -X POST -H 'Content-Type: application/json' -d '{"target": "sample" }' http://localhost:9001/search |jq '.'
```
this will respond with
```json
[
  "sample:ClientJobsIdle",
  "sample:ReqIdle",
  "sample:ReqMaxRun",
  "sample:StatusIdleOther",
  "sample:StatusStageOut",
  "sample:ClientGlideIdle",
  "sample:ClientGlideRunning",
  "sample:ClientInfoAge",
  "sample:StatusHeld",
  "sample:StatusPending",
  "sample:StatusRunning",
  "sample:ClientJobsRunning",
  "sample:StatusIdle",
  "sample:ClientGlideTotal",
  "sample:StatusStageIn",
  "sample:StatusWait"
]
```
while logging shows the data was produced by DB select:
```sh
docker logs $IMAGE
```
```sh
Target: sample
querying the cache_table table: SELECT DISTINCT fname,ds FROM cache_table WHERE fname = ?
returned rows:
fname-1:ds-1
fname-1:ds-2
fname-1:ds-3
fname-2:ds-4
fname-2:ds-5
fname-3:ds-5
fname-42:ds-1
sample:ReqMaxRun
sample:StatusPending
percent-idle:value
sample:StatusHeld
sample:ClientJobsIdle
sample:ClientJobsRunning
...
```
one can still execute `/query` requests:
```sh
curl -X POST http://localhost:9001/query -d '
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
this will return
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
* to confirm explicitly one may simply issue `/search` with the target attribute "fname" which is not in file system but was added to `cache_table`:
```sh
curl -s -X POST -H 'Content-Type: application/json' -d '{"target": "fname-1" }' http://localhost:9001/search |jq '.'
```
```json
[
  "fname-1:ds-1",
  "fname-1:ds-2",
  "fname-1:ds-3"
]
```
### Request Details

Alternatively can send GET request:
```
curl  http://localhost:9001/search?target=sample |jq '.'
```
but it is not taking into account query string of the `GET` request for `/search`. When  Grafana interacts with SimpleJSON server it is sending `POST` requests.

### Config file
The application supports `config.yaml` to have the following format:
```yaml
---
database:
  user: 'java'
  password: 'password'
  database: 'test'
  table: 'cache_table'
  server: 'mysql_server'
  port: 3306
folders:
  collect:
    - a
    - b
  reject:
    - c
    - d
```
To test, uncomment the lines compiling and copying the `mysql_client` app then:
```sh
docker run --link mysql-server --name $IMAGE -v $(pwd)/sample/:/sample -p 9001:9000 -i $IMAGE -u java -v password -w test -x mysql-server -y 3306
```
outputs
```sh
process command line args
process config file: config.yaml
database config:
User: java
Database: test
Server: mysql_server
Table: cache_table
Port: 3306

folder scan config:
collect:
a
b
reject:
c
d
User: java
Database: test
Server: mysql-server
Table: cache_table
Port: 3306

connect to the database
ping succeeds
fname-1
fname-1
fname-1
fname-2
fname-2
fname-3
my example
2
fname-1
```
### Additional Query Parameters

In order to reduce the response to `/search` request one may define additonal headers in the custom Simple JSON Dataource
```sh
Header field "Param", Value ["app"]
```
Note that grafana shows the additional headers as password input fields:
![custom input](https://github.com/sergueik/springboot_study/blob/master/basic-go-mysql/screenshots/custom_input_capture.png)

and modify the query to make use of that (taken from console logs):
```SQL
query: SELECT DISTINCT fname,ds FROM cache_table WHERE fname LIKE 'app%' ORDER BY fname
```

this will reduce the response to only
```json
[
  "app:sample:ClientGlideIdle",
  "app:sample:ClientGlideRunning",
  "app:sample:ClientGlideTotal",
  "app:sample:ClientInfoAge",
  "app:sample:ClientJobsIdle",
  "app:sample:ClientJobsRunning",
  "app:sample:ReqIdle",
  "app:sample:ReqMaxRun",
  "app:sample:StatusHeld",
  "app:sample:StatusIdle",
  "app:sample:StatusIdleOther",
  "app:sample:StatusPending",
  "app:sample:StatusRunning",
  "app:sample:StatusStageIn",
  "app:sample:StatusStageOut",
  "app:sample:StatusWait"
]
```

Note, the custom header is not shown as header in Chrome Developer Tools detail
of neither the `http://localhost:3000/datasources/edit/1/` nor the `http://localhost:3000/api/datasources/proxy/1/search` requests, that are
posted from the browser by Grafana to itself and only later get to SimpleJSON Data Source server

To change the name of the additional parameter, use `-param` flag of the `grafana-rrd-server` application

## Release Binaries

* binaries built in alpine container, cannot run on host
```sh
 ldd mysql_client
	linux-vdso.so.1 =>  (0x00007ffcd0762000)
	libc.musl-x86_64.so.1 => not found
```
* Binaries problematic to build locally, even after upgrade
 go from archaic apt version __1.6.1__ to __1.14__ on Ubuntu __xenial__:

```sh
pushd ~/Downloads/
curl -sO https://dl.google.com/go/go1.14.1.linux-amd64.tar.gz
tar xf go1.14.1.linux-amd64.tar.gz
export GOROOT=$(pwd)/go
export PATH=${GOROOT}/bin:$PATH
```
```sh
export GOPATH=$(pwd)
export GOOS=linux
export GOARCH=amd64
export CGO_ENABLED=0
export GO111MODULE=on
sudo rm -fr github.com github.com.tar
rm -f go.mod
go mod init github.com/sergueik/basic-go-mysql
```
```sh
go build rrdserver.go
```

fails with multiple compiler errors in dependency:
```sh
# github.com/ziutek/rrd
../../../go/pkg/mod/github.com/ziutek/rrd@v0.0.3/rrd.go:46:8: undefined: i64toa
../../../go/pkg/mod/github.com/ziutek/rrd@v0.0.3/rrd.go:104:10: c.create undefined (type *Creator has no field or method create, but does have Create)
../../../go/pkg/mod/github.com/ziutek/rrd@v0.0.3/rrd.go:110:12: undefined: cstring
../../../go/pkg/mod/github.com/ziutek/rrd@v0.0.3/rrd.go:111:12: undefined: cstring
../../../go/pkg/mod/github.com/ziutek/rrd@v0.0.3/rrd.go:113:10: undefined: cstring
../../../go/pkg/mod/github.com/ziutek/rrd@v0.0.3/rrd.go:117:26: undefined: newCstring
../../../go/pkg/mod/github.com/ziutek/rrd@v0.0.3/rrd.go:132:15: undefined: newCstring
../../../go/pkg/mod/github.com/ziutek/rrd@v0.0.3/rrd.go:138:26: undefined: newCstring
../../../go/pkg/mod/github.com/ziutek/rrd@v0.0.3/rrd.go:146:9: undefined: newCstring
../../../go/pkg/mod/github.com/ziutek/rrd@v0.0.3/rrd.go:147:11: u.update undefined (type *Updater has no field or method update, but does have Update)
../../../go/pkg/mod/github.com/ziutek/rrd@v0.0.3/rrd.go:147:11: too many errors
```

* build it in custom Golang Ubuntu container

```sh
export IMAGE=basic-builder-ubuntu
docker image rm -f $IMAGE
docker build -t $IMAGE -f Dockerfile.builder-ubuntu .
```
```sh
export IMAGE=go-ubuntu
docker image rm -f $IMAGE
docker build -t $IMAGE  -f Dockerfile.build-ubuntu .
export NAME=basic-go-build
docker container rm $NAME
docker run -d --name=$NAME $IMAGE
docker cp $NAME:/build/example .
```
confirm it to work
```sh
sudo apt-get install -q librrd-dev
./example -update -u java -v password -w test -x 127.0.0.1 -y 3306
```

it logs to the console the same successful messages as the containerized did before and creates database table rows - see the validation steps above
NOTE: the same approach will not work with __bionic__ due to shared library version dependency:

```sh
./example -update -u java -v password -w test -x 127.0.0.1 -y 3306
./example: error while loading shared libraries: librrd.so.4: cannot open shared object file: No such file or directory
```
```sh
sudo apt-get install -q librrd-dev
Reading package lists...
Building dependency tree...
Reading state information...
librrd-dev is already the newest version (1.7.0-1build1).
```

### Folder Selection
* clear db
```sh
docker exec -it mysql-server mysql -P 3306 -h localhost -u java -ppassword -e " source /tmp/app/mysql-init.sql"
```
* create additional `app` and `web` and `db/server` folders and copy `sample.rrd` file there
```sh
mkdir -p sample/{web,app,db/server}
for f in web app db/server; do cp sample/sample.rrd sample/$f; done
```
* populate, rejecting the `app` folder
```sh
IMAGE=basic-go-run
docker container rm -f $IMAGE
docker run --link mysql-server --name $IMAGE -v $(pwd)/sample/:/sample -p 9001:9000 -i $IMAGE -u java -v password -w test -x mysql-server -y 3306 -update  -reject app
```
```sh
database config:
User: java
Database: test
Server: mysql_server
Table: cache_table
Port: 3306

folder scan config:
collect:
a
b
reject:
c
d
User: java
Database: test
Server: mysql-server
Table: cache_table
Port: 3306

folder scan config:
collectFlag:
collect:
none
rejectFlag: app
reject:
app
```
```sh
Updating search cache.
Connected to database.
Skip directory: app
Delete from database:"db:server:sample"
Inserted into database:"db:server:sample:ClientJobsIdle"
Inserted into database:"db:server:sample:ReqMaxRun"
Inserted into database:"db:server:sample:StatusHeld"
Inserted into database:"db:server:sample:StatusStageOut"
Inserted into database:"db:server:sample:ClientGlideIdle"
Inserted into database:"db:server:sample:ClientJobsRunning"
Inserted into database:"db:server:sample:StatusIdle"
Inserted into database:"db:server:sample:ClientInfoAge"
Inserted into database:"db:server:sample:StatusPending"
Inserted into database:"db:server:sample:StatusWait"
Inserted into database:"db:server:sample:ReqIdle"
Inserted into database:"db:server:sample:ClientGlideTotal"
Inserted into database:"db:server:sample:StatusIdleOther"
Inserted into database:"db:server:sample:StatusRunning"
Inserted into database:"db:server:sample:StatusStageIn"
Inserted into database:"db:server:sample:ClientGlideRunning"
Delete from database:"percent-idle"
Inserted into database:"percent-idle:value"
Delete from database:"percent-user"
Inserted into database:"percent-user:value"
Delete from database:"sample"
Inserted into database:"sample:StatusWait"
Inserted into database:"sample:StatusHeld"
Inserted into database:"sample:StatusIdle"
Inserted into database:"sample:StatusIdleOther"
Inserted into database:"sample:StatusPending"
Inserted into database:"sample:ReqIdle"
Inserted into database:"sample:ClientGlideIdle"
Inserted into database:"sample:ClientGlideRunning"
Inserted into database:"sample:ClientInfoAge"
Inserted into database:"sample:ClientJobsIdle"
Inserted into database:"sample:ClientGlideTotal"
Inserted into database:"sample:StatusRunning"
Inserted into database:"sample:StatusStageOut"
Inserted into database:"sample:ClientJobsRunning"
Inserted into database:"sample:ReqMaxRun"
Inserted into database:"sample:StatusStageIn"
Delete from database:"web:sample"
Inserted into database:"web:sample:ClientGlideTotal"
Inserted into database:"web:sample:StatusHeld"
Inserted into database:"web:sample:StatusStageIn"
Inserted into database:"web:sample:ClientJobsRunning"
Inserted into database:"web:sample:StatusIdle"
Inserted into database:"web:sample:StatusStageOut"
Inserted into database:"web:sample:ClientGlideIdle"
Inserted into database:"web:sample:ClientGlideRunning"
Inserted into database:"web:sample:ClientInfoAge"
Inserted into database:"web:sample:ReqIdle"
Inserted into database:"web:sample:StatusWait"
Inserted into database:"web:sample:ClientJobsIdle"
Inserted into database:"web:sample:ReqMaxRun"
Inserted into database:"web:sample:StatusIdleOther"
Inserted into database:"web:sample:StatusPending"
Inserted into database:"web:sample:StatusRunning"
Closed database connection.
Finished updating search cache.
```
 - printing some debugging info while processing

* inspect db. NOTE, relative  paths are stored, and path separators converted to Classic MacOS style:
```sh
2>/dev/null docker exec -it mysql-server mysql -P 3306 -h localhost -u java -ppassword -e "use test; SELECT * FROM cache_table";
```
```sh
+----+---------------------+------------------+--------------------+---------+
| id | ins_date            | fname            | ds                 | comment |
+----+---------------------+------------------+--------------------+---------+
|  1 | 2021-08-23 00:43:36 | fname-1          | ds-1               | NULL    |
|  2 | 2021-08-23 00:43:36 | fname-1          | ds-2               | NULL    |
|  3 | 2021-08-23 00:43:36 | fname-1          | ds-3               | NULL    |
|  4 | 2021-08-23 00:43:36 | fname-2          | ds-4               | NULL    |
|  5 | 2021-08-23 00:43:36 | fname-2          | ds-5               | NULL    |
|  6 | 2021-08-23 00:43:36 | fname-3          | ds-5               | NULL    |
|  7 | 2021-08-23 00:43:49 | db:server:sample | StatusPending      | NULL    |
|  8 | 2021-08-23 00:43:49 | db:server:sample | StatusRunning      | NULL    |
|  9 | 2021-08-23 00:43:49 | db:server:sample | ClientGlideRunning | NULL    |
| 10 | 2021-08-23 00:43:50 | db:server:sample | ClientJobsIdle     | NULL    |
| 11 | 2021-08-23 00:43:50 | db:server:sample | ClientJobsRunning  | NULL    |
| 12 | 2021-08-23 00:43:50 | db:server:sample | ReqIdle            | NULL    |
| 13 | 2021-08-23 00:43:50 | db:server:sample | StatusHeld         | NULL    |
| 14 | 2021-08-23 00:43:50 | db:server:sample | StatusIdleOther    | NULL    |
| 15 | 2021-08-23 00:43:50 | db:server:sample | ClientGlideTotal   | NULL    |
| 16 | 2021-08-23 00:43:50 | db:server:sample | ClientInfoAge      | NULL    |
| 17 | 2021-08-23 00:43:50 | db:server:sample | ReqMaxRun          | NULL    |
| 18 | 2021-08-23 00:43:50 | db:server:sample | StatusStageIn      | NULL    |
| 19 | 2021-08-23 00:43:50 | db:server:sample | StatusStageOut     | NULL    |
| 20 | 2021-08-23 00:43:50 | db:server:sample | StatusWait         | NULL    |
| 21 | 2021-08-23 00:43:50 | db:server:sample | ClientGlideIdle    | NULL    |
| 22 | 2021-08-23 00:43:50 | db:server:sample | StatusIdle         | NULL    |
| 23 | 2021-08-23 00:43:50 | percent-idle     | value              | NULL    |
| 24 | 2021-08-23 00:43:50 | percent-user     | value              | NULL    |
| 25 | 2021-08-23 00:43:50 | sample           | ClientInfoAge      | NULL    |
| 26 | 2021-08-23 00:43:50 | sample           | StatusRunning      | NULL    |
| 27 | 2021-08-23 00:43:50 | sample           | StatusStageOut     | NULL    |
| 28 | 2021-08-23 00:43:50 | sample           | ClientGlideTotal   | NULL    |
| 29 | 2021-08-23 00:43:50 | sample           | ReqIdle            | NULL    |
| 30 | 2021-08-23 00:43:50 | sample           | StatusIdle         | NULL    |
| 31 | 2021-08-23 00:43:50 | sample           | StatusIdleOther    | NULL    |
| 32 | 2021-08-23 00:43:50 | sample           | StatusStageIn      | NULL    |
| 33 | 2021-08-23 00:43:51 | sample           | ClientGlideRunning | NULL    |
| 34 | 2021-08-23 00:43:51 | sample           | ClientJobsRunning  | NULL    |
| 35 | 2021-08-23 00:43:51 | sample           | ReqMaxRun          | NULL    |
| 36 | 2021-08-23 00:43:51 | sample           | StatusWait         | NULL    |
| 37 | 2021-08-23 00:43:51 | sample           | ClientGlideIdle    | NULL    |
| 38 | 2021-08-23 00:43:51 | sample           | ClientJobsIdle     | NULL    |
| 39 | 2021-08-23 00:43:51 | sample           | StatusHeld         | NULL    |
| 40 | 2021-08-23 00:43:51 | sample           | StatusPending      | NULL    |
| 41 | 2021-08-23 00:43:51 | web:sample       | ClientJobsIdle     | NULL    |
| 42 | 2021-08-23 00:43:51 | web:sample       | StatusHeld         | NULL    |
| 43 | 2021-08-23 00:43:51 | web:sample       | StatusPending      | NULL    |
| 44 | 2021-08-23 00:43:51 | web:sample       | StatusRunning      | NULL    |
| 45 | 2021-08-23 00:43:51 | web:sample       | StatusWait         | NULL    |
| 46 | 2021-08-23 00:43:51 | web:sample       | ClientGlideTotal   | NULL    |
| 47 | 2021-08-23 00:43:51 | web:sample       | ClientInfoAge      | NULL    |
| 48 | 2021-08-23 00:43:51 | web:sample       | StatusIdle         | NULL    |
| 49 | 2021-08-23 00:43:51 | web:sample       | StatusStageIn      | NULL    |
| 50 | 2021-08-23 00:43:51 | web:sample       | ClientGlideRunning | NULL    |
| 51 | 2021-08-23 00:43:51 | web:sample       | ReqMaxRun          | NULL    |
| 52 | 2021-08-23 00:43:51 | web:sample       | ClientJobsRunning  | NULL    |
| 53 | 2021-08-23 00:43:51 | web:sample       | StatusIdleOther    | NULL    |
| 54 | 2021-08-23 00:43:51 | web:sample       | StatusStageOut     | NULL    |
| 55 | 2021-08-23 00:43:52 | web:sample       | ClientGlideIdle    | NULL    |
| 56 | 2021-08-23 00:43:52 | web:sample       | ReqIdle            | NULL    |
+----+---------------------+------------------+--------------------+---------+
```
- there is no `app`

* repeat, excluding `web` folder - left as exercise

* now collect `app`. NOTE, should also include the parent folder name (`sample`):

```sh
IMAGE=basic-go-run
docker container rm -f $IMAGE
docker run --link mysql-server --name $IMAGE -v $(pwd)/sample/:/sample -p 9001:9000 -i $IMAGE -u java -v password -w test -x mysql-server -y 3306 -update  -collect app,sample
```
- console logs not shown

Golang can link with legacy native tools such as RRDTool (librrd-dev)
which java support is not possible, which typically have Perl of Python bindings
but Perl is considered outdated and gradually harder to support and Python is considered s	low


http://mirror.mia.velocihost.net/centos/7.9.2009/os/x86_64/Packages/dejavu-sans-mono-fonts-2.33-6.el7.noarch.rpm: [Errno 14] HTTP Error 403 - Forbidden
Trying other mirror.

mysql --defaults-extra-file=/etc/mysql/debian.cn

### Build Binaries for Centos / RHEL
```
docker run --name $RRD_SERVER -v $(pwd)/sample/:/sample -p 9002:9000 -d $RRD_SERVER
curl  http://localhost:9002/search?target=sample |jq '.'
```
*  build builder image
```sh
export IMAGE=builder-centos
docker image rm -f $IMAGE
docker build -t $IMAGE -f Dockerfile.$IMAGE .
```
ignore error
```sh
http://mirror.mia.velocihost.net/centos/7.9.2009/os/x86_64/Packages/dejavu-sans-mono-fonts-2.33-6.el7.noarch.rpm: [Errno 14] HTTP Error 403 - Forbidden
Trying other mirror.
```
* build application
```sh
export IMAGE=build-centos
docker image rm -f $IMAGE
docker build -t $IMAGE -f Dockerfile.$IMAGE .
export NAME=build-centos
docker container rm $NAME
docker run -d --name=$NAME $IMAGE
docker cp $NAME:/build/example .
```
* copy to vanilla Centos 7 with rrdtool and mysql installed
* binary
```sh
scp example vagrant@192.168.99.100:
```
* also copy RRd file(s) (optional)
```sh
scp sample/sample.rrd vagrant@192.168.99.100:sample:
```
* create / update user on mariadb
(from https://www.digitalocean.com/community/tutorials/how-to-reset-your-mysql-or-mariadb-root-password)

```sh
sudo systemctl stop mariadb
sudo mysqld_safe --skip-grant-tables --skip-networking &
mysql -u root
```
in mysql shell
```sql
FLUSH PRIVILEGES;
SET PASSWORD FOR 'java'@'localhost' = PASSWORD('password');
```

if the user `java` did not exist
```sh
CREATE USER 'java'@'localhost' IDENTIFIED BY 'password';
```
restore auth
```sh
sudo kill $(sudo cat /var/run/mariadb/mariadb.pid)
sudo systemctl start mariadb
```
create DB
```sh
mysql -u java -ppassword test
```
in mysql shell (re) create the table:

```sql
DROP TABLE IF EXISTS `cache_table`;
CREATE TABLE `cache_table` (
  `id`        mediumint    NOT NULL AUTO_INCREMENT,
  `ins_date`  datetime     NOT NULL,
  `fname`     varchar(255) NOT NULL,
  `ds`        varchar(255) NOT NULL,
  `expose`    varchar(255) DEFAULT NULL,
  `comment`   varchar(255) DEFAULT NULL,
  INDEX(`FNAME`),
  PRIMARY KEY (`id`)
);

```

* Build cache
```sh
./example -update -u java -v password -w test -x 127.0.0.1 -y 3306 -verbose
database config:
User:
Database:
Server:
Table:
Port: 0

folder scan config:
collect:
reject:
User: java
Database: test
Server: 127.0.0.1
Table: cache_table
Port: 3306

folder scan config:
collectFlag:
collect:
none
rejectFlag:
reject:
none
Updating search cache.
Connected to database.
Delete from database:"sample"
Inserted into database:"sample:ReqIdle"
Inserted into database:"sample:StatusIdleOther"
Inserted into database:"sample:StatusPending"
Inserted into database:"sample:StatusRunning"
Inserted into database:"sample:StatusStageIn"
Inserted into database:"sample:StatusStageOut"
Inserted into database:"sample:StatusWait"
Inserted into database:"sample:ReqMaxRun"
Inserted into database:"sample:StatusIdle"
Inserted into database:"sample:ClientGlideTotal"
Inserted into database:"sample:ClientInfoAge"
Inserted into database:"sample:ClientJobsRunning"
Inserted into database:"sample:ClientGlideIdle"
Inserted into database:"sample:ClientGlideRunning"
Inserted into database:"sample:ClientJobsIdle"
Inserted into database:"sample:StatusHeld"
Closed database connection.
Finished updating search cache.
```
* mark few entries as `data` through mysql shell:
```sh

UPDATE cache_table SET expose= '';
UPDATE cache_table SET expose='data' where ds like '%CLIENT%';
```
* Run the server
```sh
./example -u java -v password -w test -x 127.0.0.1 -y 3306 -verbose

verify from another terminal:
```sh
curl -s -X POST -H 'Content-Type: application/json' -d '{"target": "sample" }' http://localhost:9000/search |jq '.'
```
```json
[
  "sample:ReqIdle",
  "sample:StatusIdleOther",
  "sample:StatusPending",
  "sample:StatusRunning",
  "sample:StatusStageIn",
  "sample:StatusStageOut",
  "sample:StatusWait",
  "sample:ReqMaxRun",
  "sample:StatusIdle",
  "sample:ClientGlideTotal",
  "sample:ClientInfoAge",
  "sample:ClientJobsRunning",
  "sample:ClientGlideIdle",
  "sample:ClientGlideRunning",
  "sample:ClientJobsIdle",
  "sample:StatusHeld"
]
```
* test  new header processing
```sh
curl -s -X POST -H 'Content-Type: application/json' -d '{"target": "" }' -H "Param: data" http://localhost:9001/search | jq '.'
```

```json
[
  "sample:ClientGlideIdle",
  "sample:ClientGlideRunning",
  "sample:ClientGlideTotal",
  "sample:ClientInfoAge",
  "sample:ClientJobsIdle",
  "sample:ClientJobsRunning"
]
```
```sh
curl -s -X POST -H 'Content-Type: application/json' -d '{"target": "" }' \
 -H "Param: other" http://localhost:9000/search |jq '.'
```
```json
[
  "sample:StatusHeld",
  "sample:StatusIdle",
  "sample:StatusIdleOther",
  "sample:StatusPending",
  "sample:StatusRunning",
  "sample:StatusStageIn",
  "sample:StatusStageOut",
  "sample:StatusWait"
]

```
if post request header has some other value, the result will be empty;
```sh
curl -s -X POST -H 'Content-Type: application/json' -d '{"target": "" }' \
 -H "Param: something" http://localhost:9000/search |jq '.'
```
```json
[]
```

This is useful to limit the listbox to show rrd files from a specific subdirectory. To find out count breakdown by directory use the following query:
```SQL
SELECT DISTINCT(SUBSTRING(fname,1,LOCATE(':',fname))) AS fdir, COUNT(1) FROM cache_table GROUP BY fdir;
```
 further invenory will require nested selects:

```SQL
```
one can specify alternative name of the custom hease at server start:
```sh
docker container rm -f $IMAGE
docker run --link mysql-server --name $IMAGE -v $(pwd)/sample/:/sample -p 9001:9000 -d $IMAGE  -u java -v password -w test -x mysql-server -y 3306 -verbose -param custom
```
then invoke it with approptiate header:
```sh
curl -s -X POST -H 'Content-Type: application/json' -d '{"target": "" }'  -H "Custom: data" http://localhost:9001/search |jq '.'
```
### TODO

To better support Updating the cache one may chooise to store the folder information in `cache_table` alongside with the RRD data by adding a column `folder` to the table schema
To read headers
one may also use the [rrd4j/rrd4j](https://github.com/rrd4j/rrd4j) `ConverterTest` derivative:
```java
	@Test
	public void testDsNames(String dataFilePath) throws IOException {

			final String dataFileUri = System.getProperty("os.name").toLowerCase().startsWith("windows")
					? "file:///" + dataFilePath.replaceAll("\\\\", "/")
					: "file://" + dataFilePath;
			URL url = new URL(dataFileUri);
			try {
				System.err.println("Reading : " + url.getFile());
				RrdDb rrd = RrdDb.getBuilder().setPath("test")
						.setRrdToolImporter(url.getFile())
						.setBackendFactory(new RrdMemoryBackendFactory()).build();
				for (int cnt = 0; cnt != rrd.getDsCount(); cnt++) {
					String ds = rrd.getDatasource(cnt).getName();
					System.err.println("ds: " + ds);
					Assert.assertTrue(ds != null);
				}
			} catch (IllegalArgumentException e) {
				System.err.println("Skipping invalid file: " + dataFilePath);
			}
		}
	}


```
and use a potentially faster java code for directory traversal.

### Windows Install

* The  vanilla OSS Grafana 7.3.10 https://grafana.com/grafana/download/7.3.10?edition=oss&platform=windows
comes without [SimpleJSON plugin]() which is a basically an npm package. To install one

To install plugin on a Windows machine it is useful to relocate the install to %APPDATA%
```cmd
cd "c:\Program Files\GrafanaLabs\grafana\bin"
grafana-cli.exe plugins install grafana-simple-json-datasource
```
fails with the error:
```CMD
←[31mError←[0m: ←[31m✗←[0m
failed to extract plugin archive:
could not create "..\\data\\plugins\\grafana-simple-json-datasource",
permission denied, make sure you have write access to plugin dir
```

The other option is to configure grafana from the elevated account cmd window
NOTE, after the install
```sh
installing grafana-simple-json-datasource @ 1.4.2
```
one has to restart grafana via `services.msc`
![service control app](https://github.com/sergueik/springboot_study/blob/master/basic-go-mysql/screenshots/service-control-capture.jpg)
```cmd
mkdir %localappdata%\GrafanaLabs\grafana
robocopy "c:\Program Files\GrafanaLabs\grafana"  %localappdata%\GrafanaLabs\grafana /s
```

edit configuration file `conf\defaults.ini` modify port:
```text
# The http port to use
http_port = 3001
```
When launching grafana server from user owned directory
```
cd  %localappdata%\GrafanaLabs\grafana\bin
grafana-server.exe -config ..\\conf\defaults.ini start
```
one still need see the firewall access dialog.

![firewall access prompt](https://github.com/sergueik/springboot_study/blob/master/basic-go-mysql/screenshots/firewall-prompt-capture.jpg)

For local development and testing one can click 'Cancel' and the grafana will still be launched sucessfully. In the enterprise environment it will not be so easy!
[firewall allowed applications overview](https://github.com/sergueik/springboot_study/blob/master/basic-go-mysql/screenshots/firewall-overview-capture.jpg)

Note,To stop server will need to close the console window.
Even running the `stop` command in separate console window
```cmd
cd  %localappdata%\GrafanaLabs\grafana\bin
grafana-server.exe -config ..\\conf\defaults.ini stop
```
does not stop it

The information about the plugin
```cmd
%LOCALAPPDATA%\GrafanaLabs\grafana\data\plugins\grafana-simple-json-datasource
├───css
├───img
└───partials
```

is saved in the `data_source` table in `data\grafana.db` which is an sqlite database, and the mechanism of generating the `uid` column value `yiPsAB4nk` is unknown.
```cmd
sqlite3.exe ..\data\grafana.db "select * from data_source"
1|1|1|grafana-simple-json-datasource|SimpleJson|proxy|||||0|||1|{}|2021-09-01 22:43:04|2021-09-01 22:43:04|0|{}|0|yiPsAB4nk
```
NOTE: the build __1.4.0__ of
[Grafana Simple JSON Datasource](https://github.com/grafana/simple-json-datasource) plugin has a defect making it useless with __grafana-rrd-server__

![broken listbox](https://github.com/sergueik/springboot_study/blob/master/basic-go-mysql/screenshots/broken_listbox_capture.png)
- make sure to install  at least version __1.4.1__
(version __1.4.2__ is the latest available at the time of this development sprint)


the verson can be found in `data\plugins\grafana-simple-json-datasource\dist\plugin.json` or `data\plugins\grafana-simple-json-datasource\plugin.json`  (file location has changed with release)
```json
{
  "name": "SimpleJson",
  "id": "grafana-simple-json-datasource",
  "type": "datasource",
  "partials": {
    "config": "public/app/plugins/datasource/simplejson/partials/config.html"
  },
  "metrics": true,
  "annotations": true,
  "info": {
    "description": "simple json datasource",
    "author": {
      "name": "Grafana Labs",
      "url": "https://grafana.com"
    },
    "logos": {
      "small": "img/simpleJson_logo.svg",
      "large": "img/simpleJson_logo.svg"
    },
    "links": [
      {
        "name": "GitHub",
        "url": "https://github.com/grafana/simple-json-datasource"
      },
      {
        "name": "MIT License",
        "url": "https://github.com/grafana/simple-json-datasource/blob/master/LICENSE"
      }
    ],
    "version": "1.4.2",
    "updated": "2020-07-31"
  },
  "dependencies": {
    "grafanaVersion": "3.x.x",
    "plugins": []
  }
}
```
### See Also

   * https://stackoverflow.com/questions/47577385/error-non-standard-import-github-com-go-sql-driver-mysql-in-standard-package/67431068#67431068
   * https://stackoverflow.com/questions/53682247/how-to-point-go-module-dependency-in-go-mod-to-a-latest-commit-in-a-repo/
   * https://github.com/golang/go/wiki/Modules#how-to-upgrade-and-downgrade-dependencies
   * https://stackoverflow.com/questions/21743841/how-to-avoid-annoying-error-declared-and-not-used
   * another custom [mysql driver](https://github.com/s1s1ty/go-mysql-crud)
   * another possible alternative to avoid extra server during developmenrt [sqlite](https://github.com/bvinc/go-sqlite-lite)
   * https://www.digitalocean.com/community/tutorials/how-to-install-go-and-set-up-a-local-programming-environment-on-ubuntu-18-04
   * [building Go без доступа в Интернет](https://www.cyberforum.ru/go/thread2792276.html)(in Russian)
   * [MySQL transactions in Golang](https://www.sohamkamani.com/golang/sql-transactions/)
   * [blog](https://dev.to/ilyakaznacheev/a-clean-way-to-pass-configs-in-a-go-application-1g64) on __go__ way doing application configs
   * another [blog](https://peter.bourgon.org/go-best-practices-2016/#configuration) on __go__ idiosyncracies
   * java `Files.walk` [example](https://github.com/mkyong/core-java/blob/master/java-io/src/main/java/com/mkyong/io/api/FilesWalkExample.java)
   * [documentation](https://docs.oracle.com/javase/tutorial/essential/io/walk.html)
   * [Golang: Walk Directory](http://xahlee.info/golang/golang_walk_dir.html)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

