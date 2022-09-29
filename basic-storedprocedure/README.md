### Info

this directorry containes  a replica of custom String template callable storedprocedue JDBC class project
[spring-boot-call-store-procedure](https://github.com/srigalamilitan/spring-boot-call-store-procedure)
upgraded to Spring 2.x

### Usage
* start mysql in Docker container 
on host  `192.168.0.64`:
```sh
IMAGE=$(docker container ls -a | grep mysql|head -1 |awk '{print $NF}')
echo $IMAGE
```
- the `$IMAGE` may be the one assigned to container or a Docker fancy name like `sweet_zhukovsky`

```sh
docker container start $IMAGE
docker logs -f $IMAGE
```
wait for the status line:
```text
usr/sbin/mysqld: ready for connections. Version: '8.0.18'  socket: '/var/run/mysqld/mysqld.sock'  port: 3306  MySQL Community Server - GPL.
```
and update the `src/main/resources/alication.yml` with the continet node address:
```yaml
---
spring:
  dbhost: 192.168.0.64
  database: test
  datasource:
    url: jdbc:mysql://${dbhost}/${database}
    username: root
    password: root
    driver-class-name: com.mysql.jdbc.Driver
  jpa:
    database: mysql
    hibernate:
      ddl-auto: update
    show-sql: true
  jackson:
    serialization:
      indent-output: true
```
* inspect the container to confirm the credentials to use:
```sh
docker container inspect $IMAGE | jq '.[0].Config.Env'
```
this will print
```json

  "MYSQL_ROOT_PASSWORD=password",
  "MYSQL_USER=java",
  "MYSQL_DATABASE=test",
  "MYSQL_PASSWORD=password",
  "PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin",
  "GOSU_VERSION=1.7",
  "MYSQL_MAJOR=8.0",
  "MYSQL_VERSION=8.0.18-1debian9"
]
```
* connect into `mysql` shell on the container:
```sh
docker exec -it $IMAGE mysql -u root -ppassword test
```

```text
Type 'help;' or '\h' for help. Type '\c' to clear the current input statement.

mysql>
```
* create dummy stored proedure and function in the instance
```sql

DELIMITER $$

USE `test`$$

DROP PROCEDURE IF EXISTS `create_product`$$

CREATE DEFINER=`root`@`localhost` PROCEDURE `create_product`(id VARCHAR(255), p_code VARCHAR(255),p_name VARCHAR(255),weight BIGINT)
BEGIN
	
	INSERT INTO product(id, CODE,NAME,weight) VALUES(id,p_code,p_name,weight);
    END$$

DELIMITER ;
```
this will confirm with
```text
Query OK, 0 rows affected, 1 warning (0.04 sec)
```

create runction
```sql
DELIMITER $$

USE `test`$$

DROP FUNCTION IF EXISTS `count_product`$$

CREATE DEFINER=`root`@`localhost` FUNCTION `count_product`() RETURNS BIGINT(20)
BEGIN
	DECLARE v_count BIGINT DEFAULT 0;
    
	SELECT  COUNT(1) INTO v_count FROM product;
	RETURN v_count;
    END$$

DELIMITER ;
```
(the example taken from the original project sans the data base name). This will print an error:
```text
ERROR 1418 (HY000): This function has none of DETERMINISTIC, NO SQL, or READS SQL DATA in its declaration and binary logging is enabled (you *might* want to use the less safe log_bin_trust_function_creators variable)
```
- amend the definition as covered in [stackoverflow](https://stackoverflow.com/questions/26015160/deterministic-no-sql-or-reads-sql-data-in-its-declaration-and-binary-logging-i):
```sql
SET GLOBAL log_bin_trust_function_creators = 1;
DELIMITER $$

USE `test`$$

DROP FUNCTION IF EXISTS `count_product`$$

CREATE DEFINER=`root`@`localhost` FUNCTION `count_product`() RETURNS BIGINT(20)
BEGIN
	DECLARE v_count BIGINT DEFAULT 0;
    
	SELECT  COUNT(1) INTO v_count FROM product;
	RETURN v_count;
    END$$

DELIMITER ;
```
* create table manually and set `ddl`
```sql
create_product`(id VARCHAR(48), code VARCHAR(255), name VARCHAR(255),weight BIGINT);
```
* run generic test
```sh
mvn test
```
* launch app
```sh
mvn -Dmaven.test.skip=true spring-boot:run
```

if the error is observed
```text
o.s.b.f.support.DisposableBeanAdapter    : Invocation of destroy method failed on bean with name 'entityManagerFactory': 
javax.persistence.PersistenceException: [PersistenceUnit: default] 
Unable to build Hibernate SessionFactory; nested exception is 
java.lang.IllegalArgumentException: jdbcUrl is required with driverClassName.	
```
replace `src/main/resources/application.yml` with `src/main/resources/application.FIXED.yml`

- it appears there is a YAML config parser issue to address

if seeing 
```text
java.net.ConnectException: Connection refused: connect
```
check if Dockerized mysql server ports are exposed and re-create containeer otherwise.
```sh
docker container stop $IMAGE
docker container rm $IMAGE
docker run --name mysql-server -e MYSQL_ROOT_PASSWORD=password -e MYSQL_USER=java -e MYSQL_DATABASE=test -e MYSQL_PASSWORD=password -p 3306:3306 -d mysql:8.0.18
```
```sh
docker logs -f $IMAGE
```
* test the invocation of the stored procedure
```sh
curl -s -XPOST http://localhost:8080/api/product-sp/sp-create-product-jdbc -d '{"name":"product", "code":"xyz","weight":10.0}' -H "Content-type: application/json"
```
```text
success
```

and function
```sh
curl -s http://localhost:8080/api/product-sp/sp-count-product
```
```text
2
```
### See Also

  * https://github.com/seregamorph/morejdbc
  * https://github.com/malvern/spring-boot-mysql-stored-procedure
  * [Stored Procedure call from JDBC](https://www.tutorialspoint.com/springjdbc/springjdbc_stored_procedure.htm)
  * [stackoverlow](https://stackoverflow.com/questions/9361538/spring-jdbc-template-for-calling-stored-procedures) listing available ways

  * [gist](https://gist.github.com/rajkumarpb/e8e7b80b417b2cda4c1e0beda567ef29)
  * [gist](https://gist.github.com/aziz781/1336459)
  * https://stackoverflow.com/questions/49088847/after-spring-boot-2-0-migration-jdbcurl-is-required-with-driverclassname
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)


