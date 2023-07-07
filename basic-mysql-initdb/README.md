### Info

Basic docker-compose with  init script execising the auto initialization feature covered
in the mysl docker hub [documentation](https://hub.docker.com/_/mysql):

 * When a container is started for the first time, a new database with the the name specified by the `MYSQL_DATABASE` variable is being created and  initialized with the provided configuration variables.  Furthermore, mysql executes files with extensions `.sh`, `.sql` and `.sql.gz` that are found in `/docker-entrypoint-initdb.d`. Files will be executed in alphabetical order. it is frequent to mount a SQL dump into that directory


### Usage
```sh
docker pull mysql:5.7
```
```sh
export COMPOSE_HTTP_TIMEOUT=600
docker-compose up --build
```

at the end of the initialization it will log to console
```text
mysql-server    | 2023-07-06T14:44:27.925243Z 0 [Note] mysqld: ready for connections.
mysql-server    | Version: '5.7.42'  socket: '/var/run/mysqld/mysqld.sock'  port: 3306  MySQL Community Server (GPL)

```
 * verify
```sh
docker-compose ps
```
```text
    Name                 Command             State              Ports

--------------------------------------------------------------------------------
mysql-server   docker-entrypoint.sh mysqld   Up      0.0.0.0:3306-
                                                     >3306/tcp,:::3306-
                                                     >3306/tcp, 33060/tcp
```
* verify schema

```sh
docker-compose exec mysql-server sh
```

```sh
mysql -u example_db_user -pexample_db_pass
```
```sql
use example_db;
```
```text
Reading table information for completion of table and column names
You can turn off this feature to get a quicker startup with -A
Database changed
```
```sql
show tables;
```
```text
+----------------------+
| Tables_in_example_db |
+----------------------+
| example_table        |
+----------------------+
1 row in set (0.00 sec)
```
```sql
describe example_table;
```
```text
+----------+--------------+------+-----+---------+-------+
| Field    | Type         | Null | Key | Default | Extra |
+----------+--------------+------+-----+---------+-------+
| id       | bigint(20)   | NO   | PRI | NULL    |       |
| INS_DATE | datetime     | NO   |     | NULL    |       |
| NAME     | varchar(255) | NO   |     | NULL    |       |
| VALUE    | varchar(255) | YES  |     | NULL    |       |
+----------+--------------+------+-----+---------+-------+
4 rows in set (0.00 sec)
```
```sh
select count(1) from example_table;
```
```text
+----------+
| count(1) |
+----------+
|        9 |
+----------+
1 row in set (0.00 sec)
```
### Cleanup
```sh
docker-compose stop
docker-compose rm -f
```

### Note

on Windows __Docker Toolbox__ hosted environment one can find the database not completely initialize
```sql
use example_db;
show tables;
```
return
```text
Empty set (0.00 sec)
```
To fix is avoid using `volumes` in `docker-compose.yml`, but copy the directory via `Dockerfle` instead
#### TODO

the auto-initialization scenario may not work with alpine builds (not tested)

### See Also

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
