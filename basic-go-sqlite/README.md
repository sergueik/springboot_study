### Info

basic project based on code from [codeprojec article](https://www.codeproject.com/Articles/5261771/Golang-SQLite-Simple-Example)


### Usage

NOTE: repository debian/packages on Ununtu are somewhat old, __1.10.x__ for  Bionic __18.04LTS__ and  __1.6.x__ for Xenial __16.04__, archive based installs are easily doable

```sh
export GOOS=linux
export GOARCH=amd64
export GO111MODULE=on
go get -u github.com/mattn/go-sqlite3
go build -ldflags '-s -w' -o example example.go
```
 * execute
```sh
 ./example -db example.db
```
```sh
2021/08/29 05:44:55 Creating database file: example.db
2021/08/29 05:44:55 database example.db created
Create student table...
2021/08/29 05:44:55 student table created
2021/08/29 05:44:55 Inserting student record ...
2021/08/29 05:44:55 Inserting student record ...
2021/08/29 05:44:55 Inserting student record ...
2021/08/29 05:44:55 Inserting student record ...
2021/08/29 05:44:55 Inserting student record ...
2021/08/29 05:44:55 Inserting student record ...
2021/08/29 05:44:55 Inserting student record ...
2021/08/29 05:44:55 Inserting student record ...
2021/08/29 05:44:55 Inserting student record ...
2021/08/29 05:44:55 Student:  0004   Alayna Armitage   PHD
2021/08/29 05:44:55 Student:  0006   Derrick Griffiths   Master
2021/08/29 05:44:55 Student:  0002   Glen Rangel   Bachelor
```
### Filtering Redundant Data

This [link](https://www.sqlitetutorial.net/sqlite-date/) reviews various options to store datetime in SQLite.

Test the packing of data using dummy table:
```sql
drop table data;
CREATE TABLE data (
  "id" integer NOT NULL PRIMARY KEY AUTOINCREMENT,
  "code" integer,
  datetime_text,
  -- e.g. YYYY-MM-DD HH:MM:SS.SSS
  "name" TEXT );
```
filled with data
```sql
insert into data (name,code,datetime_text) values ('A',1,datetime('now'));
insert into data (name,code,datetime_text) values  ('A',2,datetime('now'));
insert into data (name,code,datetime_text) values  ('A',3,datetime('now'));
insert into data (name,code,datetime_text) values   ('B',1,datetime('now'));
insert into data (name,code,datetime_text) values   ('B',2,datetime('now')) ;
insert into data (name,code,datetime_text) values  ('B',3,datetime('now'));
insert into data (name,code,datetime_text) values   ('B',4,datetime('now'));
insert into data (name,code,datetime_text) values   ('C',1,datetime('now'));
insert into data (name,code,datetime_text) values   ('C',2,datetime('now'));
insert into data (name,code,datetime_text) values   ('D',1,datetime('now'));
```
Below are two eqivalent ways of selecting latest inderted `fname`, `ds` from several ordering by date extracted from `TEXT` column `datetime_text`.
NORE: the date extraction from `datetime_text` is assuming input in a different locale
```sql
select distinct name, code, max (printf('%s-%s-%s', substr(datetime_text, length(datetime_text) + 1, -4), substr(datetime_text, instr(datetime_text, '.') + 1, 2), substr(datetime_text, 1, 2))) from data group by name, code order by name, code;
```
```sql
select  * from data  group by name, code having  max (printf('%s-%s-%s', substr(datetime_text, length(datetime_text) + 1, -4), substr(datetime_text, instr(datetime_text, '.') + 1, 2), substr(datetime_text, 1, 2))) order by name, code
```

### See Also
  * [PSSQLite PowerShell Module](https://github.com/RamblingCookieMonster/PSSQLite) - used `System.Data.SQLite.dll` and `SQLite.Interop.dll` internally, so is platform-dependent
  * [golang sqlite3 driver](https://github.com/mattn/go-sqlite3)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

