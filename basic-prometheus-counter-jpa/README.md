### Info


this directory contains a JPA based version of [Java Node Exporter proxy for Prometheus](https://github.com/sergueik/springboot_study/tree/master/basic-prometheus-counter)
### Note

Currently if the JPA /SQL returns a null value for one of the labels, e.g. the `app`, the  Prometheus metric is created with a blank value:
```text
rpm{instance="hostname05",dc="dummy",app="",env="instance01",} 100.0
```

the code adding of Gauges with same name but varying number of labels array is not available yet.

### REGEXP Syntax Extension
#### SQLite
* this is work in progress: the `REGEX` syntax is recognized in the SQLite DB Browser but fails throough JDBC:
```sh
sudo apt-get install sqlite3-pcre
```
### MySQL
verify in console (sans JDBC):
```sh
export DATABASE_SERVER=mysql-server
docker container start $DATABASE_SERVER
docker exec -it $DATABASE_SERVER mysql -P 3306 -h localhost -u java -ppassword
```


```sql
 describe server;
```
```text
+-------+-------------+------+-----+---------+----------------+
| Field | Type        | Null | Key | Default | Extra          |
+-------+-------------+------+-----+---------+----------------+
| sid   | int(11)     | NO   | PRI | NULL    | auto_increment |
| sname | varchar(40) | NO   |     | NULL    |                |
+-------+-------------+------+-----+---------+----------------+
```
```sql
 select * from server  where CONVERT(`sid`, CHAR) REGEXP '(5|6)';
```
```text
+-----+------------+
| sid | sname      |
+-----+------------+
|   5 | hostname04 |
|   6 | hostname05 |
+-----+------------+
```

```sql
select * from server  where sname REGEXP '(hostname04|hostname05|hostname06)';
```
```text
+-----+------------+
| sid | sname      |
+-----+------------+
|   5 | hostname04 |
|   6 | hostname05 |
+-----+------------+
```
NOTE:

```sql
select * from server  where REGEXP_MATCH(sname, '(hostname04|hostname05|hostname06)');
```
```text
ERROR 1305 (42000): FUNCTION test.REGEXP_MATCH does not exist
```

```sql
select * from server  where REGEXP_LIKE(sname, '(hostname04|hostname05|hostname06)');
```
```text
+-----+------------+
| sid | sname      |
+-----+------------+
|   5 | hostname04 |
|   6 | hostname05 |
+-----+------------+
```

* invoking the request
`http://192.168.0.64:8080/serversregexp?keys=hostname00,hostname01,hostname02`
leads to the query template being logged in application console log:
```text
Hibernate: select s.sid serverId, s.sname serverName from server s where s.sname REGEXP ?
```
or 
```text
Hibernate: SELECT s.sid serverId, s.sname serverName FROM server s WHERE REGEXP_LIKE(sname, ? )
```
- the latter closer resemble Google BigQuery Regex and Pattern Matching
and result returned:
```text
[{"serverId":1,"serverName":"hostname00"},{"serverId":2,"serverName":"hostname01"},{"serverId":3,"serverName":"hostname02"}]
```

Note, using *native* SQL in Hibernate (NOTE the placeholder notation):
```java
select s.sid serverId, s.sname serverName from server s where s.sname REGEXP ?1
```

collecting raw data
```java
@Query(nativeQuery = true, value = "select s.sid serverId, s.sname serverName from server s where s.sname REGEXP ?1")
public List<Object[]> findServersNativeRegexp(String serverNamesRegexp);
```
or
```java
@Query(nativeQuery = true, value = "SELECT s.sid serverId, s.sname serverName FROM server s WHERE REGEXP_LIKE(sname, ?1 )")
public List<Object[]> findServersNativeRegexpRawData(String serverNamesRegexp);
```
and building the object on the fly in the controller:
```java
List<Server> payload = dao.findServersNativeRegexp(serverNamesRegexp)
			.stream()
			.map(columns -> new Server((int) columns[0], (String) columns[1]))
			.collect(Collectors.toList());
```

### Troubleshooting

if seeing in application log
```
2022-10-20 11:17:43.132  WARN 2016 --- [         task-1] o.h.e.j.e.i.JdbcEnvironmentInitiator     : HHH000342: Could not obtain connection to query metadata
Caused by: com.mysql.cj.exceptions.CJCommunicationsException: Communications link failure
Caused by: java.net.ConnectException: Connection refused: connect
```
and unable to connect -  attempt
```sh
docker exec -it mysql-server mysql -P 3306 -h localhost -u java -ppassword
```
see
```
cannot exec in a stopped state: unknown
```
check

```sh
docker logs mysql-server
```


if seeing 
```text
2022-10-19T19:03:23.569123Z 0 [System] [MY-011323] [Server] X Plugin ready for connections. Socket: '/var/run/mysqld/mysqlx.sock' bind-address: '::' port: 33060
mbind: Operation not permitted
mbind: Operation not permitted
mbind: Operation not permitted
mbind: Operation not permitted
mbind: Operation not permitted
mbind: Operation not permitted
mbind: Operation not permitted

```
and if unable to stop the container, need to recycle it and recreate database and table. Also may like to reboot the development host.

### Strongly Typed

Untyped query `http://localhost:8080/serversregexp?keys=hostname00,hostname01,hostname02&strict=false` works
```text
Hibernate: SELECT s.sid serverId, s.sname serverName FROM server s WHERE REGEXP_LIKE(sname, ? )
```
Typed query `http://localhost:8080/serversregexp?keys=hostname00,hostname01,hostname02&strict=true` throws exception:
```text

nested exception is org.hibernate.exception.SQLGrammarException: could not extract ResultSet] 
with root cause
java.sql.SQLSyntaxErrorException: 
You have an error in your SQL syntax; check the manual that corresponds to your 
MySQL server version for the right syntax to use near '.projection.Server(s.sid, s.sname) FROM server s WHERE s.sname REGEXP '(hostname' 
at line 1
```


The query value is
```SQL
SELECT new example.projection.Server(s.sid, s.sname) FROM server s WHERE s.sname REGEXP ?1
```

The parameter is
```java
"(hostname00|hostname0|hostname02)"
```
- works just fine with untyped query.  There is no table or column with the  name `example` on database side. Apparently MySQL is trying to 
resolve the `.projection` as some kind of schema which it cannot recognize.

### Dump the Data

```sh
mysqldump  -h 127.0.0.1 -P 3306 -u java -ppassword test > src/test/reources/test.database.txt
```
### See Also

  * [BigQuery Regex and Pattern Matching: 6 Major Things](https://hevodata.com/learn/bigquery-regex)
  * [How to ](https://chartio.com/resources/tutorials/how-to-implement-sqls-like-operator-in-google-bigquery/#using-regular-expressions) Implement SQL's LIKE Operator in Google BigQuery
  * [MySQL Regexp](https://dev.mysql.com/doc/refman/8.0/en/regexp.html)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)


