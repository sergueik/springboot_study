### Info

this directory contains JPA Hibernate table join example project with pure annotation based methods and also with concrete implementing class (one will have to check commit history)

### Testing

```sh
docker build -t mysql-server-alpine -f Dockerfile.mysql-server-alpine .
```
and run it with environments matching the `application.properties`:
```sh
export MYSQL_USER='java'
export MYSQL_PASSWORD='password'
docker run --name mysql-server-alpine -p 3306:3306 -e MYSQL_DATABASE=join_check -e MYSQL_USER=$MYSQL_USER -e MYSQL_PASSWORD=${MYSQL_PASSWORD} -e MYSQL_ROOT_PASSWORD=password -d mysql-server-alpine
```
or simply
```sh
docker start mysql-server-alpine
```
* create database and tables:
```sh
docker exec -it mysql-server-alpine mysql -P 3306 -h localhost -u java -ppassword
```

if the container was already run you may enconter
```text
ERROR 1045 (28000): Access denied for user 'java'@'localhost' (using password: YES)
```
then

```sh
docker exec -it mysql-server-alpine mysql
```
```sql

create database test;
use test;
```
```sql
drop table if exists customer;

create table customer(
  cid bigint primary key ,
  cname NVARCHAR(30) not null
);
```
```sql
drop table if exists item;

create table item(
  iid bigint primary key ,
  iname NVARCHAR(10) not null,
  iprice  bigint,
  cid bigint,
  CONSTRAINT fk_cid FOREIGN KEY (cid)
   REFERENCES customer(cid)
);
```

```sql
drop table if exists address;

create table address(
  aid bigint primary key ,
  astreet NVARCHAR(30) not null,
  acity NVARCHAR(10) not null,
  astate NVARCHAR(10) not null,
  azipcode NVARCHAR(6) not null,
  cid bigint,
  CONSTRAINT fk_acid FOREIGN KEY (cid)
   REFERENCES customer(cid)
);

```
add data, with correct foreign key
```sql
insert into customer(cname,cid)  values ('michael',1001);

insert into item(iname,iid,cid,iprice)  values ('test',201,1001,123);

select c.cname, i.iName,i.iprice from customer c  join item i;
exit;
```
 follow with one more insert

```sql
use test;
insert into customer(cname,cid)  values ('bill',1002);
```

* verify
```sql
use test;
select c.cname, i.iname,i.iprice from customer c join item i;
```
```text

+---------+-------+--------+
| cname   | iname | iprice |
+---------+-------+--------+
| michael | test  |    123 |
| bill    | test  |    123 |
+---------+-------+--------+
```
do not add rows to `address` table yet. Note its schema  will be updated by hibernate:

```text

2022-05-13 13:37:03.023  INFO 3656 --- [         task-1] org.hibernate.dialect.Dialect            : HHH000400: Using dialect: org.hibernate.dialect.MySQL5Dialect
Hibernate: alter table address add column aid bigint not null
Hibernate: alter table address add column acity varchar(50) not null
Hibernate: alter table address add column astate varchar(50) not null
Hibernate: alter table address add column astreet varchar(250) not null
Hibernate: alter table address add column azipcode varchar(10) not null
Hibernate: alter table item add constraint FK3n69su88aiehavpt9bm4cyw5j foreign key (cid) references customer (cid)
```

add data to `address`:
```sql
insert into address (acity,astreet,astate,azipcode,aid,cid) values ('atlanta','','','',301,1001),('seatle','','','',302,1002);
```
NOTE:
```
select c.cname, a.acity, i.iname,i.iprice from customer c left join item i join address a ;
```
is failing in MySQL console attempt with error:
```
ERROR 1064 (42000): You have an error in your SQL syntax; check the manual that corresponds to your MySQL server version for the right syntax to use near '' at line 1
```

one has to specify the `on` part explicitly:
```sql
use test;
select c.cname, a.acity, i.iname,i.iprice from customer c left join item i on c.cid = i.cid join address a  on c.cid = a.cid ;
```
```text
+---------+---------+-------+--------+
| cname   | acity   | iname | iprice |
+---------+---------+-------+--------+
| michael | atlanta | test  |    123 |
| bill    | seatle  | NULL  |   NULL |
+---------+---------+-------+--------+
```

```sql
select c.cname, a.acity,  i.iname,i.iprice from customer c inner join item i on c.cid = i.cid join address a on c.cid = a.cid ;
```
```text
+---------+---------+-------+--------+
| cname   | acity   | iname | iprice |
+---------+---------+-------+--------+
| michael | atlanta | test  |    123 |
+---------+---------+-------+--------+
```

* run app
```sh
mvn spring-boot:run
```

* test endpoints

```sh
curl -s  http://localhost:8080/data | /c/tools/jq-win64.exe '.'
```
will log SQL
```SQL
select customer0_.cname as col_0_0_, addresses2_.acity as col_1_0_, items1_.iname as col_2_0_, items1_.iprice as col_3_0_ from customer customer0_ left outer join item items1_ on customer0_.cid=items1_.cid inner join address addresses2_ on customer0_.cid=addresses2_.cid
```
and return JSON
```json
[
  [
    "michael",
    "atlanta",
    "test",
    123
  ],
  [
    "bill",
    "seatle",
    null,
    null
  ]
]
```
```sh
curl -s  http://localhost:8080/items | /c/tools/jq-win64.exe '.'
```

will log SQL
```SQL
select customer0_.cname as col_0_0_, addresses2_.acity as col_1_0_, items1_.iname as col_2_0_, items1_.iprice as col_3_0_ from customer customer0_ inner join item items1_ on customer0_.cid=items1_.cid inner join address addresses2_ on customer0_.cid=addresses2_.cid
```
and return JSON

```JSON
[
  {
    "customerName": "michael",
    "customerCity": "atlanta",
    "itemName": "test",
    "price": 123
  }
]

```
```sh
curl -s  http://localhost:8080/names | /c/tools/jq-win64.exe '.'
```
will log SQL
```SQL
select customer0_.cname as col_0_0_ from customer customer0_
```
and return JSON
```JSON
[
  [
    "michael"
  ],
  [
    "bill"
  ]
]
```

```sh
curl -s  http://localhost:8080/customers | /c/tools/jq-win64.exe '.'
```
will log SQL:
```text
select customer0_.cid as cid1_1_, customer0_.cname as cname2_1_ from customer customer0_
select items0_.cid as cid4_2_0_, items0_.iid as iid1_2_0_, items0_.iid as iid1_2_1_, items0_.iname as iname2_2_1_, items0_.iprice as iprice3_2_1_ from item items0_ where items0_.cid=?
select addresses0_.cid as cid6_0_0_, addresses0_.aid as aid1_0_0_, addresses0_.aid as aid1_0_1_, addresses0_.acity as acity2_0_1_, addresses0_.astate as astate3_0_1_, addresses0_.astreet as astreet4_0_1_, addresses0_.azipcode as azipcode5_0_1_ from address addresses0_ where addresses0_.cid=?
select items0_.cid as cid4_2_0_, items0_.iid as iid1_2_0_, items0_.iid as iid1_2_1_, items0_.iname as iname2_2_1_, items0_.iprice as iprice3_2_1_ from item items0_ where items0_.cid=?
select addresses0_.cid as cid6_0_0_, addresses0_.aid as aid1_0_0_, addresses0_.aid as aid1_0_1_, addresses0_.acity as acity2_0_1_, addresses0_.astate as astate3_0_1_, addresses0_.astreet as astreet4_0_1_, addresses0_.azipcode as azipcode5_0_1_ from address addresses0_ where addresses0_.cid=?
```
- note the number of SQL queries performed by Hibernate in this case - two extra queries from `addresse` and `item` per every row returned from `customer` table

and returns the JSON:
```JSON
[
  {
    "customerId": 1001,
    "customerName": "michael",
    "items": [
      {
        "itemId": 201,
        "itemName": "test",
        "price": 123
      }
    ],
    "addresses": [
      {
        "addressId": 301,
        "street": "",
        "city": "atlanta",
        "state": "",
        "zipcode": ""
      }
    ]
  },
  {
    "customerId": 1002,
    "customerName": "bill",
    "items": [],
    "addresses": [
      {
        "addressId": 302,
        "street": "",
        "city": "seatle",
        "state": "",
        "zipcode": ""
      }
    ]
  }
]
```
```sh
curl http://localhost:8080/customer/1001| jq '.'
```
will log SQL:

```text
select customer0_.cid as cid1_1_, customer0_.cname as cname2_1_ fromcustomer customer0_ where customer0_.cid=?
select items0_.cid as cid4_2_0_, items0_.iid as iid1_2_0_, items0_.iid as iid1_2_1_, items0_.iname as iname2_2_1_, items0_.iprice as iprice3_2_1_ from item items0_ where items0_.cid=?
select addresses0_.cid as cid6_0_0_, addresses0_.aid as aid1_0_0_, addresses0_.aid as aid1_0_1_, addresses0_.acity as acity2_0_1_, addresses0_.astate as astate3_0_1_, addresses0_.astreet as astreet4_0_1_, addresses0_.azipcode asazipcode5_0_1_ from address addresses0_ where addresses0_.cid=?
```
- note the number of SQL queries performed by Hibernate in this case
and return JSON:
```JSON
[
  {
    "customerId": 1001,
    "customerName": "michael",
    "items": [
      {
        "itemId": 201,
        "itemName": "test",
        "price": 123
      }
    ],
    "addresses": [
      {
        "addressId": 301,
        "street": "",
        "city": "atlanta",
        "state": "",
        "zipcode": ""
      }
    ]
  }
]

```
#### Note

* can not return collection of strongly typed objects from left join:
replacing `inner join` token in `@Query` annotation below
```java
@Query("SELECT new example.projection.CustomerItem(c.customerName, a.city,i.itemName,i.price)"
			+ " from Customer c left outer join c.items i join c.addresses a")
	public List<CustomerItem> findAllCustomerItems();
```

will lead the request
```sh
curl http://localhost:8080/items
```
to trigger exception:
```text
java.lang.IllegalArgumentException: org.hibernate.QueryException: could not instantiate class [example.projection.CustomerItem] from tuple
```
when serializing then null part of the join

### Native SQL

* this was temporarily removed from hed revision
```sh
curl http://localhost:8080/nativecust/1001 | /c/tools/jq-win64.exe  '.'
```

will result in
```JSON
[
  {
    "customerName": "michael",
    "customerCity": "atlanta",
    "abbreviation": "A",
    "itemName": "test",
    "price": 123
  },
  {
    "customerName": "michael",
    "customerCity": "atlanta",
    "abbreviation": "B",
    "itemName": "test",
    "price": 123
  }
]

```
and the native SQL will be logged to console:
```text
Hibernate:
    select
        c.cname,
        a.acity,
        i.iname,
        i.iprice,
        'A'
    from
        customer c
    join
        item i
            on c.cid = i.cid
    join
        address a
            on c.cid = a.cid
    where
        c.cid = ?
    union
    all  select
        c.cname,
        a.acity,
        i.iname,
        i.iprice,
        'B'
    from
        customer c
    join
        item i
            on c.cid = i.cid
    join
        address a
            on c.cid = a.cid
    where
        c.cid = ?
```
### Valid SQL Failing as HQL

#### Union
* execute the following [valid](https://www.mysqltutorial.org/sql-union-mysql.aspx) query in mysql console (constructed from HQLgenerated queries)
```sql
use test;
```
```sql
select
  customer0_.cname as col_0_0_,addresses2_.acity as col_1_0_,items1_.iname as col_2_0_,items1_.iprice as col_3_0_,'A' as col_4_0_
from
  customer customer0_
inner join
  item items1_ on customer0_.cid=items1_.cid
inner join
  address addresses2_ on customer0_.cid=addresses2_.cid
where
  customer0_.cid = 1001
union all
select
  customer0_.cname as col_0_0_,addresses2_.acity as col_1_0_,items1_.iname as col_2_0_,items1_.iprice as col_3_0_,'B' as col_4_0_
from
  customer customer0_
inner join
  item items1_ on customer0_.cid=items1_.cid
inner join
  address addresses2_ on customer0_.cid=addresses2_.cid
where
  customer0_.cid = 1001;
```
get back expected result
```text
+----------+----------+----------+----------+----------+
| col_0_0_ | col_1_0_ | col_2_0_ | col_3_0_ | col_4_0_ |
+----------+----------+----------+----------+----------+
| michael  | atlanta  | test     |      123 | A        |
| michael  | atlanta  | test     |      123 | B        |
+----------+----------+----------+----------+----------+
```

* attempt the equivalent in HQL:
```java
@SuppressWarnings("unchecked")
Query<Object[]> query = session
    .createQuery(
    " select c.customerName, a.city, i.itemName, i.price, 'A' from Customer c join c.items i join c.addresses a where c.customerId = :customerId"
  + " union all "
  + " select c.customerName, a.city, i.itemName, i.price, 'B' from Customer c join c.items i join c.addresses a where c.customerId = :customerId")
    .setParameter("customerId", customerId);
```
* run
```sh
mvn spring-boot:run
```
```sh
curl http://localhost:8080/cust/1001
```
* get exception in runtime
```
org.hibernate.hql.internal.ast.QuerySyntaxException:
unexpected token: union near line 1, column 155
[ select c.customerName, a.city, i.itemName,i.price, 'A' from example.model.Customer c join c.items i join c.addresses a  where c.customerId = :customerId union all  select c.customerName, a.city, i.itemName, i.price, 'B' from example.model.Customer c join c.items i join c.addresses a  where c.customerId = :customerId]
at org.hibernate.hql.internal.ast.QuerySyntaxException.convert(QuerySyntaxException.java:74)
```

#### String Functions with Arguments


All but trivial mySQL String function e.g.
```SQL
if(a.city like 'atlanta', 'c', 's') city
```
or
```SQL
egexp_replace(a.city, 'atlanta', 'a')
```
 with or without computed column alias `as city` lead to error in runtime:

 ```text
java.lang.IllegalArgumentException:
org.hibernate.QueryException:
No data type for node: org.hibernate.hql.internal.ast.tree.MethodNode
```
with the following ASCII art presumable describing tne grammar lookahead parser state when stoped condition building AST
```text
 +-[METHOD_CALL] MethodNode: '('
 | +-[METHOD_NAME] IdentNode: 'if' {originalText=if}
 | \-[EXPR_LIST] SqlNode: 'exprList'
 [select c.customerName, if (a.city like 'atlanta', 'c', 's') as city,
 i.itemName,i.price from example.model.Customer c join c.items i join
 c.addresses a where c.customerId = :customerId ]] with root cause
```
the trivial functions like `trim(a.city)` work in HQL

#### If

Adding the MySQL `if ...else` construct to HQL query string
```sql
select c.customerName, if (a.city like 'atlanta', 'c', 's') as city, i.itemName,i.price from example.model.Customer c join c.items i join c.addresses a where c.customerId = :customerId ]]
```
leads to exception in runtime with root cause
```text
org.hibernate.QueryException: No data type for node:
org.hibernate.hql.internal.ast.tree.MethodNode
+-[METHOD_CALL] MethodNode: '('
| +-[METHOD_NAME] IdentNode: 'if' {originalText=if}
| \-[EXPR_LIST] SqlNode: 'exprList'
at
org.hibernate.hql.internal.ast.tree.SelectClause.initializeExplicitSelectClause(SelectClause.java:161)
```
#### Specifying  Join column

adding the join column information explicitly
```sql
on c.customerId = a.cid
```
where `Address` is a property of `Customer` leads toexception in runtime
```text
 org.hibernate.hql.internal.ast.QuerySyntaxException: could not resolve
 property: cid of: example.model.Address [select c.customerName, a.city, i.itemName,i.price from example.model.Customer c join c.items i join c.addresses a on c.customerId = a.cid where c.customerId = :customerId ]
```

#### Discovering Field Names

Cannot dynamically extract result metadata column names required to produce a targetClass instance with specicic properties set through reflection.
This is [said](https://stackoverflow.com/questions/2605385/using-sql-column-names-in-hibernate-createsqlquery-result) to work for SQLQuery
(Attempt of using `AliasToEntityMapResultTransformer` on HQL query without specifying `aliases` returns index value as key)

```java
try {
Query query = session.createSQLQuery(
  "select c.customerName, a.city, i.itemName,i.price from Customer c join c.items i join c.addresses a where c.customerId = :customerId ")
  .setParameter("customerId", customerId);

  query.setResultTransformer(AliasToEntityMapResultTransformer.INSTANCE);
  List<Map<String, Object>> aliasToValueMapList = query.list();
```

The exception is:

```text
javax.persistence.PersistenceException:
could not extract ResultSet

with root cause
org.hibernate.exception.SQLGrammarException:
could not extract ResultSet

with root cause
SQL Error: 1142, SQLState: 42000
java.sql.SQLSyntaxErrorException:
 SELECT command denied to user 'java'@'192.168.0.25' for table 'items'
```
while in mysql logs  on server one sees
```text
Aborted connection 52 to db: 'test' user: 'java' host: '192.168.0.25'
(Got an error reading communication packets)
```
#### Limit

Addding the 
```SQL
 limit ?1
```

to each of the HQL  queries annotations lead to errors in runtime during applicaion initizalization:
```text
LocalContainerEntityManagerFactoryBean : Initialized JPA EntityManagerFactory for persistence unit 'default'
antlr.NoViableAltException: unexpected token: limi
```
and
```text
java.lang.IllegalArgumentException: Validation failed for query for method public abstract java.util.Collection example.repository.CustomerRepository.findAllCustomers(int)!
```

### SQLite

* run SQL (minor syntax differences compared to MySQL) :

```SQL
create table customer(
  cid bigint primary key ,
  cname NVARCHAR(30) not null
);
```
```SQL
create table item(
  iid bigint primary key ,
  iname NVARCHAR(10) not null,
  iprice  bigint,
  cid bigint,
  FOREIGN KEY(cid) REFERENCES customer(cid)
);
```
```SQL
create table address(
  aid bigint primary key ,
  astreet NVARCHAR(30) not null,
  acity NVARCHAR(10) not null,
  astate NVARCHAR(10) not null,
  azipcode NVARCHAR(6) not null,
  cid bigint,
  FOREIGN KEY(cid) REFERENCES customer(cid)
);

```
this will create tables
* add similar data
```sql
insert into customer(cname,cid)  values ('michael',1001);

insert into item(iname,iid,cid,iprice)  values ('test',201,1001,123);
insert into customer(cname,cid)  values ('bill',1002);
insert into address (acity,astreet,astate,azipcode,aid,cid) values ('atlanta','','','',301,1001),('seatle','','','',302,1002);

insert into customer(cname,cid)  values ('steve',1003);

insert into item(iname,iid,cid,iprice)  values ('mac',202,1003,456);

insert into address (acity,astreet,astate,azipcode,aid,cid) values ('san francisco','','','',303,1003)

```

### See Also

  * [discussion of multi-database Hibernate App fix](https://qna.habr.com/q/1104464) (in Russian)
  * [examples](https://www.programcreek.com/java-api-examples/?api=org.hibernate.query.NativeQuery) of Hibernate `NativeQuery`
  * [discussion](https://stackoverflow.com/questions/18958614/union-to-jpa-query) of workaround solution for `UNION` which  SQL supports , but JPA 2.0 JPQL does not
  * https://www.javacodegeeks.com/2018/12/java-streaming-jdbc-resultset-csv.html
  * https://narayanatutorial.com/java-tutorial/java-opencsv/csv-file-writing-resultset
  * [MySQL alpine](https://github.com/ipburger/mysql-alpine.docker/blob/master/Dockerfile)
  * https://github.com/mariuszs/spring-boot-querydsl
  * https://github.com/okihouse/spring-jpa-querydsl-sample
  * [projections](https://www.baeldung.com/jpa-hibernate-projections)
  * [projections](https://www.baeldung.com/spring-data-jpa-projections)
  * [documntation](https://www.baeldung.com/hibernate-query-to-custom-class) how to make JPA populate `Object[]` data into a custom class using  the fully qualified name of the `Result` class constructed just for the cause
  * [hibernate projection example](https://www.onlinetutorialspoint.com/hibernate/hibernate-projection-example.html)
  * https://thorben-janssen.com/hibernate-tip-left-join-fetch-join-criteriaquery/
  * [documentation](https://www.baeldung.com/spring-data-jpa-query) on basics of Spring Data JPA `@Query`


### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
