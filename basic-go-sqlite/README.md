### Info

basic project based on code from [codeprojec article](https://www.codeproject.com/Articles/5261771/Golang-SQLite-Simple-Example)


### Usage

NOTE: repository debian /1packages on Ununtu are somewhat old, __1.10.x__ for  Bionic __18.04LTS__ and  __1.6.x__ for Xenial __16.04__, archive based installs are easily doable

```sh
export GOOS=linux
export GOARCH=amd64
export GO111MODULE=on
go get -u github.com/mattn/go-sqlite3
go build -ldflags '-s -w' -o example example.go
```
 * execute
```sh
 ./example  -db example.db
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
### See Also
  
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

