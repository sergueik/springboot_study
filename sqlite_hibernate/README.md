m### Info

This directory contains a basic springboot hibernate on sqlite project based on
[restart1025/Spring-Boot-SQLite](https://github.com/restart1025/Spring-Boot-SQLite)

### Run application

Compile and start as a regular spring-boot appplication
```sh
mvn -Dmaven.test.skip=true clean spring-boot:run
```
To verify it works, access application in Postman or curl
```sh
curl http://localhost:8080/springboot/getUsers
```
initially it will respond with an empty array. 
After user(s) added, as shown below, it begins responding with something like

```json
[{
  "id": 5,
  "userName ": name,
  "passWord ": password,
  "
  "userGender ": gender,
  "nickName ": 	null
}]
```
where `name`, `passord`, `gender` will match what is inserted
```sh
curl -X POST -H "application/x-www-form-urlencoded" -d "userName=Michael&nickName=michaeljackson&gender=MAN&password=thriller&confirmPassword=thriller" http://localhost:8080/springboot/addUser
```
will respond with
```sh
User added
```
while the
```sh
curl -X POST -H "Content-Type: application/json" -d '{"userName":"John", "password":"beatles", "gender":"MAN"}' http://localhost:8080/springboot/addUserObject
```
would response with
```
{"id":10,"userName":"John","password":"beatles","gender":"MAN","nickName":null}

```
#### Database Settings

To run with persistent database, set in `application.yml`
```yaml
  datasource:
    driver-class-name: org.sqlite.JDBC
    url: jdbc:sqlite:${USERPROFILE}\\sqlite\\springboot.db
    username:
    password:
  jpa:
    database-platform: org.utils.sqlite.SQLiteDialect
    hibernate:
      ddl-auto: update
    show-sql: true
```

then create the sqlite database directory
```sh
pushd ~
mkdir sqlite
```
of
```cmd
cd /d %USERPROFILE%
MKDIR sqlite
cd sqlite
DEL /q springboot.db
sqlite3.exe springboot.db -cmd "CREATE TABLE `user` ( `id`integer, `nick_name`varchar, `pass_word`varchar, `user_gender`integer,  PRIMARY KEY(`id`) ); " ""
```
This command will create database file `~/sqlite/springboot.db` with table
```sql
CREATE TABLE `user` (
	`id`	integer,
	`nick_name`	varchar,
	`pass_word`	varchar,
	`user_name`	varchar,
	`user_gender`	integer,
	PRIMARY KEY(`id`)
);
```
alternarively can create database e.g. `testUser.db` in [SQLite browser](https://sqlitebrowser.org), with table `user`:
```sql
CREATE TABLE `user` ( `id`	integer, `nick_name` varchar, `pass_word` varchar, `user_name` varchar, `user_gender` integer, PRIMARY KEY(`id`));
```
NOTE: the `sqlite3.exe` does not work from [git bash shell](https://gitforwindows.org) on some Windows platforms.

To run database in-memory, modify settings in `application.yaml` like:
```yaml
  datasource:
    driver-class-name: org.sqlite.JDBC
    url: 'jdbc:sqlite::memory:'
    username:
    password:
```
Note: this is also the only option to workaround the excepton
```sh
Invocation of init method failed; nested exception is java.lang.IllegalArgumentException: No Spring Session store is configured: set the 'spring.session.store-type' property 
```
and other exceptions from code being of a prototype "alpha" quality
If you like the java code to create schema right before starting the app, add the `src/main/resources/hibernate.cfg.xml`
```xml
<hibernate-configuration>
  <session-factory>
    <property name="hibernate.dialect">org.utils.sqlite.SQLiteDialect</property>
    <property name="hibernate.connection.driver_class">org.sqlite.JDBC</property>
    <property name="hibernate.connection.url">jdbc:sqlite::memory:</property>
    <property name="hibernate.connection.username"/>
    <property name="hibernate.connection.password"/>
    <property name="hibernate.hbm2ddl.auto">create</property>
    <property name="hibernate.show_sql">true</property>
    <mapping class="org.utils.springboot.User "/>
  </session-factory>
</hibernate-configuration>
```
and uncomment the DDL code in `SpringbootApplication.java`:

```java
SessionFactory sessionFactory = new Configuration().configure().buildSessionFactory();

Session session = sessionFactory.openSession();
session.beginTransaction();
String query = String.format(
    "CREATE TABLE `user` ( `id`	integer, `nick_name` varchar, `pass_word` varchar, `user_name` varchar, `user_gender` integer, PRIMARY KEY(`id`));");
session.createSQLQuery(query);
Transaction transaction = session.getTransaction();
transaction.commit();
session.close();
```
NOTE: this initialization code is not working well when bundled with a Hibernate application
due to a race condition with Spring attempting to load the `User` class and need to be moved into stadalond application (this is Work in progress).

### Cleanup

```sh
docker rm -v $(docker ps -aq -f status=exited)
```
### Unit Testing
```sh
mvn clean test
```
Note: the combination of setup

```java
@Mock
private UserRepository mockRepository;

@InjectMocks
private UserController controller;
mvc = MockMvcBuilders.standaloneSetup(controller).build();
```
define the  collaborator behavior
```java
when(mockRepository.findAll()).thenReturn(Arrays.asList(new User[] { user }));
```
and define expectation that call to collaborator is taking place:
```java
```
when in `UserController` the mapping is
```java
@GetMapping("/getUsers")
public List<User> getUsers() {
	return userRepository.findAll();
}

```
then the following would be successful
```java
mvc.perform(get("/getUsers")).andExpect(content().string(containsString(new Gson().toJson(user))));
```

NOTE: with complex `User` object such test is a bit fragile: relies on serialization. 
E.g. the following is possible:
```
Expected: a string containing 
"{
  \"userName\": \"name\",
  \"password\": \"password\",
  \"gender\": \"MAN\",
  \"nickName\": \"nickname\",
  \"id\": 42
}"
but: 
was "[
  {
    \"id\": 42,
    \"userName\": \"name\",
    \"password\": \"password\",
    \"gender\": \"MAN\",
    \"nickName\": \"nickname\"
  }
]"
```
- note a subtle differnce in JSON keys order

### TODO

* after conversion to spring boot parent __2.3.4.RELEASE__ along with many 
tricky API changes the project passes tests but fails in runtime with
```text
org.springframework.transaction.CannotCreateTransactionException: 
Could not open JPA EntityManager for transaction; nested exception is 
java.lang.NoSuchMethodError: 
org.springframework.orm.jpa.JpaTransactionManager$JpaTransactionObject.setReadOnly(Z)V] 
with root cause
java.lang.NoSuchMethodError: 
org.springframework.orm.jpa.JpaTransactionManager$JpaTransactionObject.setReadOnly(Z)V
	at org.springframework.orm.jpa.JpaTransactionManager.doBegin(JpaTransactionManager.java:405) 
  ~[spring-orm-5.2.9.RELEASE.jar:5.2.9.RELEASE]
```

### See also

  * [Hibernate/DAO basics](https://habrahabr.ru/post/255829/) (in russian)
  * [diyfr/sqlite-dialect](https://github.com/diyfr/sqlite-dialect)
  * [Spring Boot Reference Guide](https://docs.spring.io/spring-boot/docs/current/reference/html/howto-build.html)
  * [xerial/sqlite-jdbc](https://bitbucket.org/xerial/sqlite-jdbc)
  * [tools and libraries download](https://www.sqlite.org/download.html)
  * sqlite3 [command syntax](https://www.sqlite.org/cli.html)
  * [Spring Boot - JPA Hibernate MySQL](https://github.com/alicankustemur/spring-boot-jpa-hibernate-mysql-example) project - unsuccessfully tried to get converted to SQLite hibernate backend
  * collection of [JPA projects](https://github.com/AnghelLeonard/Hibernate-SpringBoot)
  * [externalize](https://mkyong.com/hibernate/how-to-load-hibernate-cfg-xml-from-different-directory/) the `hibernate.cfg.xml`
  * another [solution](https://stackoverflow.com/questions/27508327/design-for-hibernate-external-config-for-app-server-and-local-eclipse)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
