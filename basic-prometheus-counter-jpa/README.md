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
invoking the request
`http://192.168.0.64:8080/serversregexp?keys=hostname00,hostname01,hostname02`
leads to the query template being logged in application console log:
```
Hibernate: select s.sid serverId, s.sname serverName from server s where s.sname REGEXP ?
```
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
and building the object on the fly in the controller:
```java
List<Server> payload = dao.findServersNativeRegexp(serverNamesRegexp)
			.stream()
			.map(columns -> new Server((int) columns[0], (String) columns[1]))
			.collect(Collectors.toList());
```
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

