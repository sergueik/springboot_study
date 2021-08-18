###  Info
example from https://github.com/go-sql-driver/mysql/wiki/Example
### Usage
*  have mysql container up
```sh
docker pull mysql:8.0.18
```
and run it with environments matching the  hardcoded values in `example.go`:
```sh
docker run --name mysql-server -e MYSQL_ROOT_PASSWORD=password -e MYSQL_USER=java -e MYSQL_DATABASE=test -e MYSQL_PASSWORD=password -d mysql:8.0.18
```
The enviroment entries `MYSQL_ROOT_PASSWORD`, `MYSQL_USER`,`MYSQL_DATABASE`, `MYSQL_PASSWORD` are required by Mysql docker image.
It will take the Docker instance  quite some time to launch. 
One can safely start building and runing Spring app container while database initializes itself.
Eventually
observe the successful start log message in `mysql-server` container:
```sh
docker logs mysql-server
```
*  compile go program and copy locally

```sh
export IMAGE=basic-go-build
docker image rm -f $IMAGE
docker build -t $IMAGE -f Dockerfile.build .
export NAME=basic-go-build
docker container rm $NAME
docker run -d --name=$NAME $IMAGE
docker cp $NAME:/app/example .
```
```sh
IMAGE=basic-go-run
docker build -t $IMAGE -f Dockerfile.run  .
docker run --link mysql-server  -v /etc:/tmp/etc -it $IMAGE /example
```
### Troubleshooting

```sh
panic: Error 1045: Access denied for user 'user'@'172.17.0.3' (using password: YES)

goroutine 1 [running]:
main.main()
        /app/example.go:18 +0x17d

```
 verify can connect locally on `mysql-server`:
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
### See Also

  * [sqlite](https://github.com/bvinc/go-sqlite-lite)
  * another custom [mysql driver](https://github.com/s1s1ty/go-mysql-crud)
   * https://stackoverflow.com/questions/47577385/error-non-standard-import-github-com-go-sql-driver-mysql-in-standard-package/67431068#67431068
