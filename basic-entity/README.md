# Info
this directory contains repica of
[web link crawler with SQLIte JPA](https://github.com/patzu/Crawler) to tune and explore customizations
### USage
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
create table hibernate_sequences (sequence_name varchar(255) not null, next_val bigint, primary key (sequence_name))
```
### See Also
https://stackoverflow.com/questions/59900419/inject-collection-name-in-document-from-spring-data-mongodb-in-using-a-property
https://shekhargulati.com/2018/01/09/programmatically-generating-database-schema-with-hibernate-5/
https://stackoverflow.com/questions/49148172/table-name-configured-with-external-properties-file
