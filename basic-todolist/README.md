### Info

This directory contains a replica of [todolist](https://github.com/skkovalenko/ToDoList)
project: basic Springboot web database project

```sh
docker pull mysql:8.0.18
```
and run it with environments matching the `application.properties`:
```sh
docker run --name mysql-server -e MYSQL_ROOT_PASSWORD=password -e MYSQL_USER=java -e MYSQL_DATABASE=todolist -e MYSQL_PASSWORD=password -d mysql:8.0.18
```
possibly have to create schema ?

```sh
docker logs mysql-server
```
```sh
docker exec -it mysql-server mysql -P 3306 -h localhost -u java -ppassword
```
this falls into interactive mysql client shell:
```
mysql> show databases;
+--------------------+
| Database           |
+--------------------+
| information_schema |
| todolist           |
+--------------------+
```
```sh
use todolost;
show tables;
```
this will initially show
```
Empty set (0.00 sec)
```
Will need to create a empty table there:
```sh
CREATE TABLE todo (id INTEGER, name VARCHAR(20), description VARCHAR(20), date DATE); 
```
```
Query OK, 0 rows affected (0.44 sec)
mysql> \q
```
* Build the `todolist-example` Docker image
```sh
docker build -f Dockerfile -t todolist-example .
```
* Lanch the `todolist-example` backed Docker container
```sh
docker run -p 8086:8086 -e "SERVICE-PORT=3306" --link mysql-server -d todolist-example
```

The attempt to open `http://127.0.0.1:8086/todo-list/` in the browser leads to a unmasked exeption
```sh
There was an unexpected error (type=Internal Server Error, status=500).
could not extract ResultSet; SQL [n/a]; nested exception is org.hibernate.exception.SQLGrammarException: could not extract ResultSet
```

with the inner exception shown in container console log:
```sh
nested exception is
org.springframework.dao.InvalidDataAccessResourceUsageException: could not extract ResultSet; 
SQL [n/a]; nested exception is org.hibernate.exception.SQLGrammarException: 
could not extract ResultSet] with root cause
java.sql.SQLSyntaxErrorException: Table 'todolist.todo' doesn't exist
```
or
```sh
org.hibernate.HibernateException: The database returned no natively generated identity value

```
That iplies the table was not created.
If `http://127.0.0.1:8086/todo-list/`shows
```sh
[]
```
this implies it builds as REST not WEB project
If error [](https://stackoverflow.com/questions/49813666/table-dbname-hibernate-sequence-doesnt-exist)
```
java.sql.SQLSyntaxErrorException: Table 'todolist.hibernate_sequence
```
is thrown on insert
### See Also
  * original [post](https://habr.com/ru/post/496386/) (in Russian). 
