### Info

__PostgreSQL on Docker: Basic to Advanced__ [tutorial post](https://habr.com/ru/articles/578744/)(in Russian, possibly translated)

### Usage

```sh
mkdir data
```
may have to stop postgresql if running localy
```sh
sudo netstat -antp | grep 5432
```
```text
tcp        0      0 0.0.0.0:5432            0.0.0.0:*               LISTEN      1152/postgres 
```


```sh
sudo /etc/init.d/postgresql  stop
```
* see the failure when file is used in command line in volume argument:
```sh
IMAGE=postgres:9.6-alpine3.13
NAME=postgresql-test
docker run --name $NAME -p 5432:5432 -e POSTGRES_USER=habrpguser -e POSTGRES_PASSWORD=pgpwd4habr -e POSTGRES_DB=habrdb -e PGDATA=/var/lib/postgresql/data/pgdata -d -v "$(pwd)/data":/var/lib/postgresql/data -v "$(pwd)/initdb/1.sql":/docker-entrypoint-initdb.d/1.sql $IMAGE
```


```sh
docker logs $NAME
```
this will show
```text
/usr/local/bin/docker-entrypoint.sh: running /docker-entrypoint-initdb.d/1.sql
psql:/docker-entrypoint-initdb.d/1.sql:0: could not read from input file: Is a directory
```
* note, the `initdb` scripts applied alphabetically
```sh
IMAGE=postgres:9.6-alpine3.13
NAME=postgresql-test
docker container rm $NAME
docker run --name $NAME -p 5432:5432 -e POSTGRES_USER=habrpguser -e POSTGRES_PASSWORD=pgpwd4habr -e POSTGRES_DB=habrdb -e PGDATA=/var/lib/postgresql/data/pgdata -d -v "$(pwd)/data":/var/lib/postgresql/data -v "$(pwd)/initdb":/docker-entrypoint-initdb.d $IMAGE
```
this will log the warning 
```text
PostgreSQL Database directory appears to contain a database; Skipping initialization
```

![Skipping Initialization](https://github.com/sergueik/springboot_study/blob/master/basic-postgresql-docker-compose/screenshots/capture-skipping.png)

and ignore the initialization

Connect to container interactively
```sh
docker exec -it $NAME sh
```
and try to run init sql script by hand:
```sh
/ # psql -h localhost -p 5432 --username habrpguser --password -d habrdb </docker-entrypoint-initdb.d/1.sql
```
```sh
Password for user habrpguser:
```
```
could not read from input file: Is a directory
```
```sh
ls -ld /docker-entrypoint-initdb.d/1.sql
```
```txt
drwxr-xr-x    2 root     root            40 Jul  8 14:38 /docker-entrypoint-initdb.d/1.sql
```
This is apparently a limitation of __Docker ToolBox__ __Windows 8.1__

create a initialization SQL file and load it

NOTE: after the container is already running it is not helpful to create new files in the mapped directory - apparently such host files are not visible in the container, with Windows 8.1 host
```sh
echo "create table sometable(id int);"  > /docker-entrypoint-initdb.d/2.sql
```

```sh
psql -h localhost -p 5432 --username habrpguser --password -d habrdb </docker-entrypoint-initdb.d/2.sql
```

```sh
Password for user habrpguser:
```
```text
CREATE TABLE
```

```sh	
psql -h localhost -p 5432 --username habrpguser --password -d habrdb
```
```sql
\d
```
```text
            List of relations
 Schema |   Name    | Type  |   Owner
--------+-----------+-------+------------
 public | sometable | table | habrpguser
```
### Cleanup
```sh
docker stop $NAME
docker container rm $NAME
```
```sh
export COMPOSE_HTTP_TIMEOUT=600
docker-compose up --build
```
```sh
docker-compose ps
```
will first show
```text
NAME                                         COMMAND                  SERVICE       STATUS              PORTS
basic-postgresql-docker-compose-postgres-1   "docker-entrypoint.s…"   postgres      running             0.0.0.0:5432->5432/tcp 
client                                       "tail -f /dev/null"      psql-client   running (starting)
```
and later
```text
basic-postgresql-docker-compose-postgres-1   "docker-entrypoint.s…"   postgres      running             0.0.0.0:5432->5432/tcp
client                                       "tail -f /dev/null"      psql-client   running (healthy)
```
```sh
docker-compose logs postgres
```
NOTE, the warning
```text
basic-postgresql-docker-compose-postgres-1  | PostgreSQL Database directory appears to contain a database; Skipping initialization
```
and near the end of the console log
```text
basic-postgresql-docker-compose-postgres-1  | LOG:  database system is ready to accept connection
```
```sh
docker-compose exec -it postgres sh
```
* repeat verification steps
### Verify HealthCheck
* verify
```sh
CONTAINER_ID=$(docker container ls -a | grep client | awk '{print $1}')
docker exec -it $CONTAINER_ID sh
```
* in the `client` container run
```sh
cat /tmp/a.*.txt
```

* will see a number of
```text
 ?column?
----------
        1
(1 row)
```

messages in console
### Cleanup

```sh
docker-compose stop 
docker-compose rm -f
sudo rm -fr data
```
### See Also

    * long [discussion](https://gist.github.com/onjin/2dd3cc52ef79069de1faa2dfd456c945) of volume mounting and initdb /PGDATA  interplay on Docker Toolbox on Windows 8.1
    * [original post](https://qna.habr.com/q/1292232)(in Russian)
    * discussion on [stackoverflow](https://stackoverflow.com/questions/59715622/docker-compose-and-create-db-in-postgres-on-init) of `docker-compose` and `create db` in Postgres on init
    * [how to keep Docker container running indefinitely](https://devopscube.com/keep-docker-container-running/)
   * [how do I specify a password to psql non-interactively?](https://stackoverflow.com/questions/6405127/how-do-i-specify-a-password-to-psql-non-interactively)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)


