###  Info

Combination of two docker containers to practice the examples from [golang MySQL Tutorial](https://tutorialedge.net/golang/golang-mysql-tutorial/)

and subject [Wiki](https://github.com/go-sql-driver/mysql/wiki/Example)
with [grafana rrd server](https://github.com/doublemarket/grafana-rrd-server) 

Changing the code loading cache for later accessing the data in 
[RRDTool files](https://oss.oetiker.ch/rrdtool/) and implement 
SimpleJSON grafana data sources over `/search`, `query`, `annotations` [protocol](https://grafana.com/grafana/plugins/grafana-simple-json-datasource/)

### Usage
*  have mysql container up
```sh
docker pull mysql:8.0.18
```
and run it with environmenti variables matching the hard-coded values in `example.go`:
```sh
docker run -v $(pwd):/tmp/app --name mysql-server -e MYSQL_ROOT_PASSWORD=password -e MYSQL_USER=java -e MYSQL_DATABASE=test -e MYSQL_PASSWORD=password -d mysql:8.0.18
```
The required enviroment entries `MYSQL_ROOT_PASSWORD`, `MYSQL_USER`,`MYSQL_DATABASE`, `MYSQL_PASSWORD` are described in Mysql docker image.
Alternatively,
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
* Initialize DB
```sh
docker exec -it mysql-server mysql -P 3306 -h localhost -u java -ppassword -e " source /tmp/app/mysql-init.sql"
```

* build cache
```sh
docker run --link mysql-server --name $IMAGE -v $(pwd)/sample/:/sample -p 9001:9000 -i $IMAGE -update -u java -v password -w test -x mysql-server -y 3306
```

this will log to console
```sh
Updating search cache.
Connected to database.
new item:"percent-idle:value"
Inserted into database.
new item:"percent-user:value"
Inserted into database.
new item:"sample:ClientInfoAge"
...
Closed database connection.
Finished updating search cache.
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
2>/dev/null docker exec -it mysql-server mysql -P 3306 -h localhost -u java -ppassword -e "use test; SELECT * FROM cache_table";
```
```text
+----+---------------------+--------------+--------------------+---------+
| id | ins_date            | fname        | ds                 | comment |
+----+---------------------+--------------+--------------------+---------+
|  1 | 2021-08-20 17:26:44 | fname-1      | ds-1               | NULL    |
|  2 | 2021-08-20 17:26:44 | fname-1      | ds-2               | NULL    |
|  3 | 2021-08-20 17:26:44 | fname-1      | ds-3               | NULL    |
|  4 | 2021-08-20 17:26:44 | fname-2      | ds-4               | NULL    |
|  5 | 2021-08-20 17:26:44 | fname-2      | ds-5               | NULL    |
|  6 | 2021-08-20 17:26:44 | fname-3      | ds-5               | NULL    |
|  7 | 2021-08-20 17:27:13 | percent-idle | value              | NULL    |
|  8 | 2021-08-20 17:27:13 | percent-user | value              | NULL    |
|  9 | 2021-08-20 17:27:13 | sample       | StatusStageOut     | NULL    |
| 10 | 2021-08-20 17:27:13 | sample       | ClientJobsIdle     | NULL    |
| 11 | 2021-08-20 17:27:13 | sample       | ReqIdle            | NULL    |
| 12 | 2021-08-20 17:27:13 | sample       | ClientGlideRunning | NULL    |
| 13 | 2021-08-20 17:27:13 | sample       | ClientGlideTotal   | NULL    |
| 14 | 2021-08-20 17:27:13 | sample       | ClientInfoAge      | NULL    |
| 15 | 2021-08-20 17:27:13 | sample       | StatusRunning      | NULL    |
| 16 | 2021-08-20 17:27:13 | sample       | StatusIdle         | NULL    |
| 17 | 2021-08-20 17:27:13 | sample       | StatusIdleOther    | NULL    |
| 18 | 2021-08-20 17:27:14 | sample       | StatusPending      | NULL    |
| 19 | 2021-08-20 17:27:14 | sample       | StatusHeld         | NULL    |
| 20 | 2021-08-20 17:27:14 | sample       | StatusStageIn      | NULL    |
| 21 | 2021-08-20 17:27:14 | sample       | StatusWait         | NULL    |
| 22 | 2021-08-20 17:27:14 | sample       | ClientGlideIdle    | NULL    |
| 23 | 2021-08-20 17:27:14 | sample       | ClientJobsRunning  | NULL    |
| 24 | 2021-08-20 17:27:14 | sample       | ReqMaxRun          | NULL    |
+----+---------------------+--------------+--------------------+---------+
```

* start server
```sh
docker container rm -f $IMAGE
docker run --link mysql-server --name $IMAGE -v $(pwd)/sample/:/sample -p 9001:9000 -d $IMAGE  -u java -v password -w test -x mysql-server -y 3306
```
this will start web server
* try search
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
ping succeeds
querying the cache table
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

to confirm explicitly one may simply issue `/search` with the target attribute "fname" which is not in file system but was added to `cache_table`:
```sh
curl -s -X POST -H 'Content-Type: application/json' -d '{"target": "fname" }' http://localhost:9001/search |jq '.'
```
```json
[
  "fname-1:ds-1",
  "fname-1:ds-2",
  "fname-1:ds-3",
  "fname-2:ds-4",
  "fname-2:ds-5",
  "fname-3:ds-5",
  "fname-42:ds-1"
]
```
### Troubleshooting

```sh
panic: Error 1045: Access denied for user 'user'@'172.17.0.3' (using password: YES)

goroutine 1 [running]:
main.main()
        /app/example.go:18 +0x17d

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
if the connection works the hard coded credentials may be out of sync in `example.go`
The other frequent error is docker used the cache too aggressively

### Dependency Management

the grafaa-rrd-server dependencies are very specicic version of the 

Not using `go.sum` , `go.mod` from that project the leads to compile error in the build phase (see `Dockerfile.build-broken`)

```sh
docker build -t $IMAGE -f Dockerfile.build-broken .
```
```text
/go/src/github.com/ziutek/rrd/rrd.go:110:12: undefined: cstring
```
replacing 
```sh
 RUN go get -u github.com/ziutek/rrd@v0.0.3
```
with
```sh
 RUN go get -u github.com/ziutek/rrd@552b878b2633c1e8031c30a9e7d1d3aa18517061
```
or  other commits does not fix the error 
Apparently some specific commit do, but finding which has is needed is labor intensive:

```sh
export IMAGE=basic-go-build
docker image rm -f $IMAGE
docker build -t $IMAGE -f Dockerfile.build .
export NAME=basic-go-build
docker container rm $NAME
docker run -it --name=$NAME $IMAGE sh

```
explore the build container:
```sh
cd /go/pkg/mod/github.com/ziutek/rrd@v0.0.3
ls -la
```
```sh
ls -la
total 60
dr-x------    2 root     root          4096 Aug 19 15:28 .
drwxr-xr-x    3 root     root          4096 Aug 19 15:28 ..
-r--r--r--    1 root     root          1394 Aug 19 15:28 LICENSE
-r--r--r--    1 root     root           481 Aug 19 15:28 README.md
-r--r--r--    1 root     root            38 Aug 19 15:28 go.mod
-r--r--r--    1 root     root         10357 Aug 19 15:28 rrd.go
-r--r--r--    1 root     root         12273 Aug 19 15:28 rrd_c.go
-r--r--r--    1 root     root          4469 Aug 19 15:28 rrd_test.go
-r--r--r--    1 root     root          1715 Aug 19 15:28 rrdfunc.c
-r--r--r--    1 root     root           721 Aug 19 15:28 rrdfunc.h
```
(unfinshed)
### See Also

   * [sqlite](https://github.com/bvinc/go-sqlite-lite)
   * another custom [mysql driver](https://github.com/s1s1ty/go-mysql-crud)
   * https://stackoverflow.com/questions/47577385/error-non-standard-import-github-com-go-sql-driver-mysql-in-standard-package/67431068#67431068
   * https://stackoverflow.com/questions/53682247/how-to-point-go-module-dependency-in-go-mod-to-a-latest-commit-in-a-repo/
   * https://github.com/golang/go/wiki/Modules#how-to-upgrade-and-downgrade-dependencies
   * https://stackoverflow.com/questions/21743841/how-to-avoid-annoying-error-declared-and-not-used
	
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

