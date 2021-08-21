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
  `comment`   varchar(255) DEFAULT NULL,
  INDEX(`FNAME`),
  PRIMARY KEY (`id`)
) 
```
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
docker cp $NAME:/build/mysql_client .
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
Delete from database:"percent-idle"
Inserted into database:"percent-idle:value"
Delete from database:"percent-user"
Inserted into database:"percent-user:value"
Delete from database:"sample"
Inserted into database:"sample:ClientInfoAge"
Inserted into database:"sample:StatusHeld"
Inserted into database:"sample:StatusPending"
Inserted into database:"sample:StatusRunning"
Inserted into database:"sample:StatusStageIn"
Inserted into database:"sample:ClientGlideRunning"
Inserted into database:"sample:ClientGlideTotal"
Inserted into database:"sample:StatusIdle"
Inserted into database:"sample:StatusIdleOther"
Inserted into database:"sample:ClientGlideIdle"
Inserted into database:"sample:ClientJobsRunning"
Inserted into database:"sample:ReqIdle"
Inserted into database:"sample:ReqMaxRun"
Inserted into database:"sample:ClientJobsIdle"
Inserted into database:"sample:StatusStageOut"
Inserted into database:"sample:StatusWait"
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

### Release Binaries

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

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)



