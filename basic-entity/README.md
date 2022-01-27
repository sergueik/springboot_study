# Info
this directory contains repica of
[web link crawler with SQLIte JPA](https://github.com/patzu/Crawler) to tune and explore customizations
### Usage
#### Prerequisite
create SQLite database `springboot.db` on the desktop.

```sql
CREATE TABLE `product` (
  `id`  INTEGER,
  `name`  TEXT NOT NULL,
  `price`  TEXT NOT NULL,
  `description`  TEXT NOT NULL,
  `extra_information`  TEXT NOT NULL,
  PRIMARY KEY(`id`)
);
```
```sql
drop table if exists hibernate_sequences;
create table hibernate_sequences (sequence_name varchar(255) not null, next_val bigint, primary key (sequence_name));
```
* run the app
```sh
mvn spring-boot:run
```
* see it start adding information into database:
```sh

Waiting queue size: 1
Fetching data from http://magento-test.finology.com.my/breathe-easy-tank.html
66 links found.
24 links are ignored.
42 links are added to queue.
One product is added to database.
Waiting queue size: 42
Fetching data from https://magento-test.finology.com.my/breathe-easy-tank.html
66 links found.
65 links are ignored.
1 links are added to queue.
One product is added to database.
Waiting queue size: 42
Fetching data from https://magento-test.finology.com.my/customer/account/login/r
eferer/aHR0cHM6Ly9tYWdlbnRvLXRlc3QuZmlub2xvZ3kuY29tLm15L2JyZWF0aGUtZWFzeS10YW5rL
mh0bWw%2C/
```
* allow it run to see the logged message
```txt
One product is added to database.
```
* abort via CTLR-C.

* check the database
```cmd
sqlite3 "products.db" "select *  from product;"
```

### See Also
https://stackoverflow.com/questions/59900419/inject-collection-name-in-document-from-spring-data-mongodb-in-using-a-property
https://shekhargulati.com/2018/01/09/programmatically-generating-database-schema-with-hibernate-5/
https://stackoverflow.com/questions/49148172/table-name-configured-with-external-properties-file
https://github.com/mmushfiq/springboot-sqlite-mini-website
