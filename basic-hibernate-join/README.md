### Info

This directory containes the source  from [hibernate HQL join example](
https://www.onlinetutorialspoint.com/hibernate/hibernate-left-join-example.html)
updated to MySQL __8.x__
NOTE - the source code in the article is not 100% accurate. 

Run it with a stock mysql 88 container

### Usage
* build app
```
mvn clean package
```
* pull the container (pinned to some specific version to avoid polluting docker image cache locally):

```sh
docker pull mysql:8.0.18
```
and run it with environments matching the `hibernate.cfg.xml`:
```sh

docker run --name mysql-server -e MYSQL_ROOT_PASSWORD=password -e MYSQL_USER=java -p 3306:3306 -e MYSQL_DATABASE=test -e MYSQL_PASSWORD=password -d mysql:8.0.18
```
* create database and tables:
```sh
docker exec -it mysql-server mysql -P 3306 -h localhost -u java -ppassword 
```
```sql

create database test;
use test;
```
```sql
drop table if exist customer;

create table customer(
  cid bigint primary key ,
  cname NVARCHAR(30) not null,
  ccity NVARCHAR(30) not null
);
```
```sql
drop table if exist item;

create table item(
  iid bigint primary key ,
  iname NVARCHAR(30) not null,
  iprice  bigint,
  cid bigint,
  CONSTRAINT fk_cid FOREIGN KEY (cid)
   REFERENCES customer(cid)
);
```
add data, with correct foreign key 
```sql
insert into customer(cname,cid,ccity)  values ('michael',1001,'atlanta');

insert into item(iname,iid,cid,iprice)  values ('test',201,1001,123);

select c.cname, c.cCity, i.iName,i.iprice from customer c  join item i;
exit;
```
 follow with one more insert

```sql
use test;
insert into customer(cname,cid,ccity)  values ('bill',1002,'seattle');
```

* verify
```sql
use test;
select c.cname, c.ccity, i.iname,i.iprice from customer c join item i;
```
```text

+---------+---------+-------+--------+
| cname   | ccity   | iname | iprice |
+---------+---------+-------+--------+
| michael | atlanta | test  |    123 |
| bill    | seattle | test  |    123 |
+---------+---------+-------+--------+
```

NOTE:
```
select c.cname, c.ccity, i.iname,i.iprice from customer c left join item i;
```
is failing in MySQL console attempt with error:
```
ERROR 1064 (42000): You have an error in your SQL syntax; check the manual that corresponds to your MySQL server version for the right syntax to use near '' at line 1
```

one has to specify the `on` part explicitly:
```sql
use test;
select c.cname, c.ccity, i.iname,i.iprice from customer c left join item i on c.cid = i.cid;
```
```text
+---------+---------+-------+--------+
| cname   | ccity   | iname | iprice |
+---------+---------+-------+--------+
| michael | atlanta | test  |    123 |
| bill    | seattle | NULL  |   NULL |
+---------+---------+-------+--------+
```

```sql
select c.cname, c.ccity, i.iname,i.iprice from customer c inner join item i on c.cid = i.cid;
```
```text
+---------+---------+-------+--------+
| cname   | ccity   | iname | iprice |
+---------+---------+-------+--------+
| michael | atlanta | test  |    123 |
+---------+---------+-------+--------+
```

#### test from Hibernate
* update the `connection.url` attribute with the nformation relevant for the mahine where mysql is run:
```sh
xmllint -xpath "//property[@name='connection.url']/text()" src/main/resources/hibernate.cfg.xml
```java
jdbc:mysql://192.168.0.64:3306/test
```

```sh
java -cp target/example.hibernate-join.jar:target/lib/* example.Main
```
```cmd
java -cp target/example.hibernate-join.jar;target/lib/* example.Main
```

```text 
May 10, 2022 6:34:52 PM org.hibernate.annotations.common.reflection.java.JavaReflectionManager <clinit>
INFO: HCANN000001: Hibernate Commons Annotations {4.0.4.Final}
May 10, 2022 6:34:52 PM org.hibernate.Version logVersion
INFO: HHH000412: Hibernate Core {4.3.0.Final}
May 10, 2022 6:34:52 PM org.hibernate.cfg.Environment <clinit>
INFO: HHH000206: hibernate.properties not found
May 10, 2022 6:34:52 PM org.hibernate.cfg.Environment buildBytecodeProvider
INFO: HHH000021: Bytecode provider name : javassist
May 10, 2022 6:34:53 PM org.hibernate.cfg.Configuration configure
INFO: HHH000043: Configuring from resource: hibernate.cfg.xml
May 10, 2022 6:34:53 PM org.hibernate.cfg.Configuration getConfigurationInputStream
INFO: HHH000040: Configuration resource: hibernate.cfg.xml
May 10, 2022 6:34:53 PM org.hibernate.internal.util.xml.DTDEntityResolver resolveEntity
WARN: HHH000223: Recognized obsolete hibernate namespace http://hibernate.sourceforge.net/. Use namespace http://www.hibernate.org/dtd/ instead. Refer to Hibernate 3.6 Migration Guide!
May 10, 2022 6:34:53 PM org.hibernate.cfg.Configuration doConfigure
INFO: HHH000041: Configured SessionFactory: null
May 10, 2022 6:34:53 PM org.hibernate.engine.jdbc.connections.internal.DriverManagerConnectionProviderImpl configure
INFO: HHH000402: Using Hibernate built-in connection pool (not for production use!)
Loading class `com.mysql.jdbc.Driver'. This is deprecated. The new driver class is `com.mysql.cj.jdbc.Driver'. The driver is automatically registered via the SPI and manual loading of the driver class is generally unnecessary.
May 10, 2022 6:34:53 PM org.hibernate.engine.jdbc.connections.internal.DriverManagerConnectionProviderImpl buildCreator
INFO: HHH000401: using driver [com.mysql.jdbc.Driver] at URL [jdbc:mysql://localhost:3306/test]
May 10, 2022 6:34:53 PM org.hibernate.engine.jdbc.connections.internal.DriverManagerConnectionProviderImpl buildCreator
INFO: HHH000046: Connection properties: {user=root, password=****}
May 10, 2022 6:34:53 PM org.hibernate.engine.jdbc.connections.internal.DriverManagerConnectionProviderImpl buildCreator
INFO: HHH000006: Autocommit mode: false
May 10, 2022 6:34:53 PM org.hibernate.engine.jdbc.connections.internal.DriverManagerConnectionProviderImpl configure
INFO: HHH000115: Hibernate connection pool size: 20 (min=1)
May 10, 2022 6:34:53 PM org.hibernate.dialect.Dialect <init>
INFO: HHH000400: Using dialect: org.hibernate.dialect.MySQL5Dialect
May 10, 2022 6:34:53 PM org.hibernate.engine.transaction.internal.TransactionFactoryInitiator initiateService
INFO: HHH000399: Using default transaction strategy (direct JDBC transactions)
May 10, 2022 6:34:53 PM org.hibernate.hql.internal.ast.ASTQueryTranslatorFactory <init>
INFO: HHH000397: Using ASTQueryTranslatorFactory
Hibernate: select customer0_.cname as col_0_0_, customer0_.ccity as col_1_0_, items1_.iname as col_2_0_, items1_.iprice as col_3_0_ from customer customer0_ left outer join item items1_ on customer0_.cid=items1_.cid
michael -- atlanta--test--123
bill -- seattle--null--null
cleared session
closed session
```
if the error is obsrved there is possibly a firewall between

```text
Exception in thread "main" org.hibernate.exception.JDBCConnectionException: Error calling Driver#connect
Caused by:
com.mysql.cj.jdbc.exceptions.CommunicationsException: Communications link failure
```
### See Also

 * [similar project](https://github.com/ashokitschool/Hibernate-HQL-App-MultiTables) without `OneToMany` annotation in entity classes, similar `HQL`, moved implementation to custom method in the `Dao*` class
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)


