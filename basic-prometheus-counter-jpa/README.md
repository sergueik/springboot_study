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
### See Also

  * [BigQuery Regex and Pattern Matching: 6 Major Things](https://hevodata.com/learn/bigquery-regex)
  * [How to ](https://chartio.com/resources/tutorials/how-to-implement-sqls-like-operator-in-google-bigquery/#using-regular-expressions) Implement SQL's LIKE Operator in Google BigQuery
  * [MySQL Regexp](https://dev.mysql.com/doc/refman/8.0/en/regexp.html)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

