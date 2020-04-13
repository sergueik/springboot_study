### Info

This directory contains a basic springboot jdbc on postgresql project based on
[KominfoPemudaPersis/springboot-jdbc-postgres](https://github.com/KominfoPemudaPersis/springboot-jdbc-postgres)


### Run application

```sh
sudo -u postgres psql
```
```sh
ALTER USER postgres PASSWORD 'postgres';
ALTER ROLE
```
verify the credentials
```sh
psql -h localhost -p 5432  --username postgres --password
```
```sh
postgres=# \c
```
create database and table
```sh
sudo -u postgres psql
```
```sh
postgres=# create database example;
CREATE DATABASE
postgres=# \c example
You are now connected to database "example" as user "postgres".
example=# CREATE TABLE rest ( id serial PRIMARY KEY NOT NULL, key varchar(100) NOT NULL, value varchar(250) NOT NULL, rand smallint NOT NULL);
```
```sh
mvn -Dmaven.test.skip=true clean spring-boot:run
```

### Run in Docker
package jar 
```sh
mvn -Dmaven.test.skip=true clean package
```

### TODO:
The bolierplate test class is incomplete:

```sh
java.lang.IllegalStateException: Unable to find a @SpringBootConfiguration, you need to use @ContextConfiguration or @SpringBootTest(classes=...) with your test
```
### See also

 * another basic [jdbc postgress example](https://github.com/christosperis/spring-jdbctemplate-postgresql-example)
 * [initialize default user and password in postgresql](https://chartio.com/resources/tutorials/how-to-set-the-default-user-password-in-postgresql/)
 * postgresql [command rederence](https://www.tutorialspoint.com/postgresql/postgresql_select_database.htm)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
