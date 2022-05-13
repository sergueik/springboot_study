### Info 

this directory contains replica of skeleton project [multiple-db-connection](https://github.com/Java-Gyan-Mantra/multiple-db-connection) demonstrating how to configure  Springboot application to operate Mongo DB and SQLite simultaneously 

### Testing
 * create mongo server container
```sh
IMAGE=mongodb
CONTAINER=mongo-server
docker build -t $IMAGE -f Dockerfile.$IMAGE .
docker container prune -f
docker run -d --name $CONTAINER -p 27717:27017 -i $IMAGE
docker logs $CONTAINER
```

```sh
docker build -t mysql-server-alpine -f Dockerfile.mysql-server-alpine .
```
and run it with environments matching the `application.properties`:
```sh
export MYSQL_USER='java'
export MYSQL_PASSWORD='password'
docker run --name mysql-server-alpine -p 3306:3306 -e MYSQL_DATABASE=join_check -e MYSQL_USER=$MYSQL_USER -e MYSQL_PASSWORD=${MYSQL_PASSWORD} -e MYSQL_ROOT_PASSWORD=password -d mysql-server-alpine
```
* running app
```sh
mvn spring-boot:run
```
* testing
```sh
curl http://localhost:8080/products?db=zzz
```
```text
invalid operation: zzz
```
```sh
curl http://localhost:8080/products?db=mysql
```
```text
[]
```
```sh
curl http://localhost:8080/products?db=mongo
```

```text
[]
```
* add product
```sh
curl -X POST -H 'Content-Type: application/json'  http://localhost:8080/products -d '{
  "id": 123,
  "qty": 1,
  "price": 1000,
  "name": "product"
}

```

repeat queries:
```ssh
curl http://localhost:8080/products?db=mongo
```
```json
[{"id":123,"name":"product","qty":1,"price":1000.0}]
```
```sh
curl http://localhost:8080/products?db=mysql
```
```json
[{"id":0,"name":null,"qty":0,"price":0.0}]
```
-  need a fix

### Multi Table 

```sh
docker build -t mysql-server-alpine -f Dockerfile.mysql-server-alpine .
```
and run it with environments matching the `application.properties`:
```sh
export MYSQL_USER='java'
export MYSQL_PASSWORD='password'
docker run --name mysql-server-alpine -p 3306:3306 -e MYSQL_DATABASE=join_check -e MYSQL_USER=$MYSQL_USER -e MYSQL_PASSWORD=${MYSQL_PASSWORD} -e MYSQL_ROOT_PASSWORD=password -d mysql-server-alpine
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

```sql
drop table if exist address;

create table address(
  aid bigint primary key ,
  astreet NVARCHAR(30) not null,
  acity NVARCHAR(30) not null,
  astate NVARCHAR(30) not null,
  azipcode NVARCHAR(30) not null,
  cid bigint,
  CONSTRAINT fk_acid FOREIGN KEY (cid)
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


```sh
curl http://localhost:8080/cust/1001 | /c/tools/jq-win64.exe  '.'
```

```json
[
  {
    "customerId": 0,
    "customerName": "michael",
    "customerCity": "atlanta",
    "itemId": 0,
    "itemName": "test",
    "price": 123
  }
]
```
application log:
```text
positoryDao       : findCustomerDetailsByCustomerId processing customerId =1001
Hibernate:
    select
        customer0_.cname as col_0_0_,
        customer0_.ccity as col_1_0_,
        items1_.iname as col_2_0_,
        items1_.iprice as col_3_0_
    from
        customer customer0_
    inner join
        item items1_
            on customer0_.cid=items1_.cid
    where
        customer0_.cid=?
2022-05-12 20:41:18.852  INFO 6316 --- [nio-8080-exec-2] e.repository.CustomerRe
positoryDao       : michael -- atlanta--test--123

```

```sh
 curl -s http://localhost:8080/cust | /c/tools/jq-win64.exe  '.'
```
```JSON
[
  {
    "customerId": 0,
    "customerName": "michael",
    "customerCity": "atlanta",
    "itemId": 0,
    "itemName": "test",
    "price": 123
  },
  {
    "customerId": 0,
    "customerName": "bill",
    "customerCity": "seattle",
    "itemId": 0,
    "itemName": null,
    "price": 0
  }
]

```
application log:
```text

2022-05-12 20:42:52.098  INFO 6316 --- [nio-8080-exec-8] e.repository.CustomerRe
positoryDao       : findAllCustomerDetails
Hibernate:
    select
        customer0_.cname as col_0_0_,
        customer0_.ccity as col_1_0_,
        items1_.iname as col_2_0_,
        items1_.iprice as col_3_0_
    from
        customer customer0_
    left outer join
        item items1_
            on customer0_.cid=items1_.cid
2022-05-12 20:42:52.147  INFO 6316 --- [nio-8080-exec-8] e.repository.CustomerRe
positoryDao       : michael -- atlanta--test--123
2022-05-12 20:42:52.148  INFO 6316 --- [nio-8080-exec-8] e.repository.CustomerRe
positoryDao       : bill -- seattle--null--null

```
### See Also

  * [discussion of multi-database Hibernate App fix](https://qna.habr.com/q/1104464) (in Russian)
  * https://www.javacodegeeks.com/2018/12/java-streaming-jdbc-resultset-csv.html
  * https://narayanatutorial.com/java-tutorial/java-opencsv/csv-file-writing-resultset
  * [MySQL alpine](https://github.com/ipburger/mysql-alpine.docker/blob/master/Dockerfile)
