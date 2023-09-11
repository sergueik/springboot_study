### Info
this directory containes code based on [java Concurrent Collection - ConcurrentHashMap Examples](https://www.codejava.net/java-core/concurrency/java-concurrent-collection-concurrenthashmap-examples) combined into a generic Springboot app to represent caching database proxy server

### Usage

```sh
mvn spring-boot:run
```
this will start backgrond worker thread updating a memory hash with thread-specific values from the SQLite database:
```text
Hibernate: select user0_.id as id1_0_, user0_.gender as gender2_0_, user0_.nick_name as nick_nam3_0_, user0_.password as password4_0_, user0_.name as name5_0_ from user user0_
1694455352011: Writer-1 has updated users
...
```

the state of the can in-memory inventory be queried via
```sh

curl -s http://localhost:8085/all/
```

This will print the hash:
```text
1694217055820: 1=>userName: John, pasword: beatles, gender: MAN; 2=>userName: Michael, pasword: thriller, gender: MAN; 
...
```
indvidual row:
```sh
ID=10
curl -s -X GET http://localhost:8085/json/$ID
```
```JSON
{
  "id": 10,
  "userName": "Michael",
  "password": "thriller",
  "gender": "MAN"
}

```
```sh
curl -s http://localhost:8085/data/10?prop=gender
```

```text
MAN
```
```
curl -s http://localhost:8085/data/10?prop=id
```
```text
10
```
```sh
curl -s http://localhost:8085/data/10?prop=substr
```
```text
unknown
```
### TODO

The project only works with very old SpringBoot parent
__1.5.4.RELEASE__

switching to
```XML
  <parent>
    <groupId>org.springframework.boot</groupId>
    <artifactId>spring-boot-starter-parent</artifactId>
    <version>2.3.4.RELEASE</version>
    <relativePath/>
  </parent>
```
leads to startup error in runtime:
```sh
mvn spring-boot:run
```
```text
org.springframework.beans.factory.BeanCreationException: Error creating bean with name 'jpaVendorAdapter' defined in class path resource [org/springframework/boot/autoconfigure/orm/jpa/HibernateJpaConfiguration.class]: Bean instantiation via factory method failed; nested exception is org.springframework.beans.BeanInstantiationException: Failed to instantiate [org.springframework.orm.jpa.JpaVendorAdapter]: Factory method 'jpaVendorAdapter' threw exception; nested exception is java.lang.NoClassDefFoundError: org/hibernate/jpa/HibernatePersistenceProvider

```

With matching legacy versions __2.3.4.RELEASE__ and __5.3.20.Final__ the exception becomes:
```text
 Started Application in 8.543 seconds (JVM running for 9.259)
Exception in thread "Thread-3" org.springframework.transaction.CannotCreateTransactionException: Could not open JPA EntityManager for transaction; nested exception is java.lang.NoSuchMethodError: org.springframework.orm.jpa.JpaTransactionManager$JpaTransactionObject.setReadOnly(Z)V

```
 moving aroung versions does not resolve this -  see `pom.xml.BROKEN-IN-JPA`
the same error whether the database is in memory `jdbc:sqlite::memory:` or on disk `jdbc:sqlite:${HOME}/Desktop/springboot.db`


```sh
docker exec -it mysql-server mysql -P 3306 -h localhost -u java -ppassword -e 'use test ; drop table if exists `user`; CREATE TABLE `user` (  `id` int AUTO_INCREMENT  PRIMARY KEY, `nick_name` varchar(255), `gender` int, `password` varchar(255), `name` varchar(255));'
```

```sh
docker exec -it mysql-server mysql -P 3306 -h localhost -u java -ppassword -e 'use test; insert into user ( nick_name, gender, password, name) values ("ringo", 0, "starr","richard");'

```
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
