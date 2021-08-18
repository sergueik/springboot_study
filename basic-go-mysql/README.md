###  Info
Combination of two docker containers to practice the examples from [golang MySQL Tutorial](https://tutorialedge.net/golang/golang-mysql-tutorial/)

and subject [Wiki](https://github.com/go-sql-driver/mysql/wiki/Example)

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
docker build -t $IMAGE -f Dockerfile.builder .

export IMAGE=basic-go-build
docker build -t $IMAGE -f Dockerfile.build .
export NAME=basic-go-build
docker container rm $NAME
docker run -d --name=$NAME $IMAGE
docker cp $NAME:/app/example .
``
```sh
IMAGE=basic-go-run
docker build -t $IMAGE -f Dockerfile.run  .
docker run --link mysql-server -it $IMAGE /example
```
this will connect to DB running on `mysql-server`, perform various basic CRUD operations and print the results
```text
ping succeeds
example-1
example-2
example-3
example-4
example-5
example-6
example-7
example-8
example-9
my example
2
example-2
```
### Initialize DB
```sh
docker exec -it mysql-server mysql -P 3306 -h localhost -u java -ppassword -e " source /tmp/app/mysql-init.sql"
```
followed by checks
```sh
docker exec -it mysql-server mysql -P 3306 -h localhost -u java -ppassword -e  "use test; show tables;";
```
```text
+----------------+
| Tables_in_test |
+----------------+
| example_table  |
+----------------+
```
and
```sh
2>/dev/null docker exec -it mysql-server mysql -P 3306 -h localhost -u java -ppassword -e "use test; SELECT * FROM example_table";
```
```text
+----+---------------------+-----------+---------+
| id | INS_DATE            | NAME      | VALUE   |
+----+---------------------+-----------+---------+
|  1 | 2021-08-18 20:36:18 | example-1 | value-1 |
|  2 | 2021-08-18 20:36:18 | example-2 | value-2 |
|  3 | 2021-08-18 20:36:18 | example-3 | value-3 |
|  4 | 2021-08-18 20:36:18 | example-4 | value-4 |
|  5 | 2021-08-18 20:36:18 | example-5 | value-5 |
|  6 | 2021-08-18 20:36:18 | example-6 | value-6 |
|  7 | 2021-08-18 20:36:18 | example-7 | value-7 |
|  8 | 2021-08-18 20:36:18 | example-8 | value-8 |
|  9 | 2021-08-18 20:36:18 | example-9 | value-9 |
+----+---------------------+-----------+---------+

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

### See Also

  * [sqlite](https://github.com/bvinc/go-sqlite-lite)
  * another custom [mysql driver](https://github.com/s1s1ty/go-mysql-crud)
   * https://stackoverflow.com/questions/47577385/error-non-standard-import-github-com-go-sql-driver-mysql-in-standard-package/67431068#67431068
