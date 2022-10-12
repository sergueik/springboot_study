### Info

clone of the [basic console Spring boot app](https://github.com/arkenidar/java-spring-sql) with PostgreSQL JDBC connection

### Usage

use the postgresql alpine container defined in the sibling project `basic-postgresql`
stop the `postgresql` if run on the host
```sh
sudo /etc/init.d/postgresql stop
```
alternatively through `systemctl`
```
sudo systemctl stop postgresql
```
* rebuild posgressql image
```sh
SERVER_IMAGE=alpine-postgres
docker build -f Dockerfile.$SERVER_IMAGE -t $SERVER_IMAGE .
SERVER_NAME=postgres-database
docker container stop $SERVER_NAME
docker container  rm $SERVER_NAME
docker run --name $SERVER_NAME -e POSTGRES_PASSWORD=postgres -p 5432:5432 -d $SERVER_IMAGE
```
verify it is available on the host
```
sudo netstat -antp | grep 5432
```

if not check for the container errors

if the error is
```sh docker logs  postgres-database
```
```text
exec /docker-entrypoint.sh: no such file or directory
```
* to troubleshoot, run the container overriding entrypoint and command:

```sh
docker run --name $SERVER_NAME -e POSTGRES_PASSWORD=postgres -p 5432:5432 --entrypoint "" -it $SERVER_IMAGE sh
````
check the Windows line ending:

```sh
vi /docker-entrypoint.sh
```
```text
#!/bin/shM
chown -R postgres "$PGDATA"M
mkdir /run/postgresqlM
...
```
fix the same
```sh
pushd ../basic-postgresql
sed -i 's|\r||g' docker-entrypoint.sh
git add docker-entrypoint.sh
popd
```

rebuild the image


### Create the database if not exist already
```sh
psql -h localhost -p 5432 --username postgres --password
```

```SQL
select datname from pg_database;
```

```SQL
create database example;
```

```SQL
CREATE TABLE data ( id serial PRIMARY KEY NOT NULL, key varchar(100) NOT NULL, value varchar(250) NOT NULL);
```

```sh
psql -h localhost -p 5432 --username postgres --password
```

```SQL
select datname from pg_database;
```

```SQL
create database example;
```
```text
CREATE DATABASE

```
```SQL
\c example
```
```text
Password:
```
```text
You are now connected to database "example" as user "postgres".

```
```SQL
CREATE TABLE data ( id serial PRIMARY KEY NOT NULL, key varchar(100) NOT NULL, value varchar(250) NOT NULL);
```

```text
CREATE TABLE
```

insert data

```SQL
INSERT INTO data (key,value) VALUES ('f1', 'b1');
```
```SQL
INSERT INTO data (key,value) VALUES ('f2', 'b2');
```
```SQL
INSERT INTO data (key,value) VALUES ('f3', 'b3'),('f3','b4'),('f3','b5');
```

```text
CREATE TABLE
```


* run the spring booot appin colsone

```sh 
mvn spring-boot:run

```

### See Also

  * https://github.com/egalli64/jd
  * https://github.com/Kroshkazavr/data_base_service
