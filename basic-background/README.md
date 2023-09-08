### Info
this directory containes code based on [java Concurrent Collection - ConcurrentHashMap Examples](https://www.codejava.net/java-core/concurrency/java-concurrent-collection-concurrenthashmap-examples) combined into a generic Springboot app to represent caching database proxy server

### Usage

```sh
mvn spring-boot:run
```
this will start backgrond worker thread updating a memory hash with thread-specific values from the SQLite database:
```text
Hibernate: select user0_.id as id1_0_, user0_.gender as gender2_0_, user0_.nick_name as nick_nam3_0_, user0_.password as password4_0_, user0_.name as name5_0_ from user user0_
1694215757069: Writer-1 has put [1 => userName: John, pasword: beatles, gender:MAN]
1694215757153: Writer-1 has put [2 => userName: Michael, pasword: thriller, gender: MAN]
1694215757185: Writer-1 has put [3 => userName: Michael, pasword: thriller, gender: MAN]
1694215757212: Writer-1 has put [4 => userName: Michael, pasword: thriller, gender: MAN]
1694215757239: Writer-1 has put [5 => userName: Michael, pasword: thriller, gender: MAN]
1694215757268: Writer-1 has put [6 => userName: Michael, pasword: thriller, gender: MAN]
1694215757292: Writer-1 has put [7 => userName: Michael, pasword: thriller, gender: MAN]
1694215757311: Writer-1 has put [8 => userName: Michael, pasword: thriller, gender: MAN]
1694215757333: Writer-1 has put [9 => userName: Michael, pasword: thriller, gender: MAN]
1694215757355: Writer-1 has put [10 => userName: Michael, pasword: thriller, gender: MAN]
...
```

the state of the can in-memory inventory be queried via
```sh

curl -s http://localhost:8085/basic/
```

This will print the hash:
```text
1694217055820: 1=>userName: John, pasword: beatles, gender: MAN; 2=>userName: Michael, pasword: thriller, gender: MAN; 
...
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
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
