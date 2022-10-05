#### Info

replica of [spring-mysql-stored](https://github.com/Java-Gyan-Mantra/spring-mvc-stored-procedure-call) project using [Mysql on Alpine](https://github.com/wangxian/alpine-mysql) `Dockerfile` showing mysql stored procedure call from hibernate

### Usage

* build light mysql container
```sh
docker build -t mysql-server-alpine -f Dockerfile.mysql-server-alpine .
```
* run container with environment settings matching the `application.properties`:
```sh
export MYSQL_USER='java'
export MYSQL_PASSWORD='password'
docker run --name mysql-server-alpine -p 3306:3306 -e MYSQL_DATABASE=join_check -e MYSQL_USER=$MYSQL_USER -e MYSQL_PASSWORD=${MYSQL_PASSWORD} -e MYSQL_ROOT_PASSWORD=password -d mysql-server-alpine
```
* execute SQL 

```sh
docker exec -it mysql-server-alpine mysql
```

```sql
USE join_check;
DROP PROCEDURE IF EXISTS `getData`;

DELIMITER //
USE join_check//
    CREATE PROCEDURE `getData`(IN email VARCHAR(255))
        LANGUAGE SQL
        DETERMINISTIC
        SQL SECURITY DEFINER
        BEGIN
            SELECT * FROM employee WHERE email_id = email;
        END //
DELIMITER;
```
```sql
\q
```
#### See Also

https://www.baeldung.com/stored-procedures-with-hibernate-tutorial

