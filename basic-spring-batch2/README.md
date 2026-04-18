### Info

Basic docker-compose basic spring batch Demo [spring-batch](https://github.com/EalenXie/springboot-batch) downgraded to MySQL __5.7__.

### Usage

```sh
docker-compose up --build -d
```
docker-compose ps
```
```text
```
```
```sh
docker-compose exec mysql-server sh
```


```sh
mysql -u example_db_user -p
```

```sh
mysql> show databases;
```
```text
+--------------------+
| Database           |
+--------------------+
| information_schema |
| example_db         |
+--------------------+
2 rows in set (0.00 sec)
```
```sh

mysql> use example_db
```
```text
Database changed
```
```sh
mysql> show tables ;
```
```text
+------------------------------+
| Tables_in_example_db         |
+------------------------------+
| BATCH_JOB_EXECUTION          |
| BATCH_JOB_EXECUTION_CONTEXT  |
| BATCH_JOB_EXECUTION_PARAMS   |
| BATCH_JOB_EXECUTION_SEQ      |
| BATCH_JOB_INSTANCE           |
| BATCH_JOB_SEQ                |
| BATCH_STEP_EXECUTION         |
| BATCH_STEP_EXECUTION_CONTEXT |
| BATCH_STEP_EXECUTION_SEQ     |
| access                       |
+------------------------------+
10 rows in set (0.00 sec)

```
```sh
docker-compose logs app | less
```
```text
Attaching to basic-spring-batch2_app_1
app_1            | 
app_1            |   .   ____          _            __ _ _
app_1            |  /\\ / ___'_ __ _ _(_)_ __  __ _ \ \ \ \
app_1            | ( ( )\___ | '_ | '_| | '_ \/ _` | \ \ \ \
app_1            |  \\/  ___)| |_)| | | | | || (_| |  ) ) ) )
app_1            |   '  |____| .__|_| |_|_| |_\__, | / / / /
app_1            |  =========|_|==============|___/=/_/_/_/
app_1            |  :: Spring Boot ::        (v2.3.4.RELEASE)
app_1            | 
app_1            | 2026-04-18 02:01:18.716  INFO 1 --- [           main] name.ealen.SpringBatchApplication        : Starting SpringBatchApplication v0.1.0-SNAPSNOT on 2c57e0cfe8de with PID 1 (/app.jar started by root in /)
app_1            | 2026-04-18 02:01:18.727  INFO 1 --- [           main] name.ealen.SpringBatchApplication        : No active profile set, falling back to default profiles: default
app_1            | 2026-04-18 02:01:21.981  INFO 1 --- [           main] .s.d.r.c.RepositoryConfigurationDelegate : Bootstrapping Spring Data JPA repositories in DEFERRED mode.
app_1            | 2026-04-18 02:01:22.232  INFO 1 --- [           main] .s.d.r.c.RepositoryConfigurationDelegate : Finished Spring Data repository scanning in 116ms. Found 0 JPA repository interfaces.
app_1            | 2026-04-18 02:01:23.993  INFO 1 --- [           main] o.s.s.concurrent.ThreadPoolTaskExecutor  : Initializing ExecutorService 'threadPoolTaskExecutor'
app_1            | 2026-04-18 02:01:24.024  INFO 1 --- [           main] com.zaxxer.hikari.HikariDataSource       : HikariPool-1 - Starting...
app_1            | 2026-04-18 02:01:25.510 ERROR 1 --- [           main] com.zaxxer.hikari.pool.HikariPool        : HikariPool-1 - Exception during pool initialization.
app_1            | 
app_1            | com.mysql.jdbc.exceptions.jdbc4.CommunicationsException: Communications link failure
app_1            | 
app_1            | The last packet sent successfully to the server was 0 milliseconds ago. The driver has not received any packets from the server.
app_1            | 	at java.base/jdk.internal.reflect.NativeConstructorAccessorImpl.newInstance0(Native Method) ~[na:na]
app_1            | 	at java.base/jdk.internal.reflect.NativeConstructorAccessorImpl.newInstance(Unknown Source) ~[na:na]
app_1            | 	at java.base/jdk.internal.reflect.DelegatingConstructorAccessorImpl.newInstance(Unknown Source) ~[na:na]
app_1            | 	at java.base/java.lang.reflect.Constructor.newInstance(Unknown Source) ~[na:na]
app_1            | 	at com.mysql.jdbc.Util.handleNewInstance(Util.java:403) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	at com.mysql.jdbc.SQLError.createCommunicationsException(SQLError.java:990) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	at com.mysql.jdbc.MysqlIO.<init>(MysqlIO.java:335) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	at com.mysql.jdbc.ConnectionImpl.coreConnect(ConnectionImpl.java:2187) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	at com.mysql.jdbc.ConnectionImpl.connectOneTryOnly(ConnectionImpl.java:2220) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	at com.mysql.jdbc.ConnectionImpl.createNewIO(ConnectionImpl.java:2015) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	at com.mysql.jdbc.ConnectionImpl.<init>(ConnectionImpl.java:768) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	at com.mysql.jdbc.JDBC4Connection.<init>(JDBC4Connection.java:47) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	at java.base/jdk.internal.reflect.NativeConstructorAccessorImpl.newInstance0(Native Method) ~[na:na]
app_1            | 	at java.base/jdk.internal.reflect.NativeConstructorAccessorImpl.newInstance(Unknown Source) ~[na:na]
app_1            | 	at java.base/jdk.internal.reflect.DelegatingConstructorAccessorImpl.newInstance(Unknown Source) ~[na:na]
app_1            | 	at java.base/java.lang.reflect.Constructor.newInstance(Unknown Source) ~[na:na]
app_1            | 	at com.mysql.jdbc.Util.handleNewInstance(Util.java:403) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	at com.mysql.jdbc.ConnectionImpl.getInstance(ConnectionImpl.java:385) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	at com.mysql.jdbc.NonRegisteringDriver.connect(NonRegisteringDriver.java:323) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	at com.zaxxer.hikari.util.DriverDataSource.getConnection(DriverDataSource.java:138) ~[HikariCP-3.4.5.jar!/:na]
app_1            | 	at com.zaxxer.hikari.pool.PoolBase.newConnection(PoolBase.java:358) ~[HikariCP-3.4.5.jar!/:na]
app_1            | 	at com.zaxxer.hikari.pool.PoolBase.newPoolEntry(PoolBase.java:206) ~[HikariCP-3.4.5.jar!/:na]
app_1            | 	at com.zaxxer.hikari.pool.HikariPool.createPoolEntry(HikariPool.java:477) ~[HikariCP-3.4.5.jar!/:na]
app_1            | 	at com.zaxxer.hikari.pool.HikariPool.checkFailFast(HikariPool.java:560) ~[HikariCP-3.4.5.jar!/:na]
app_1            | 	at com.zaxxer.hikari.pool.HikariPool.<init>(HikariPool.java:115) ~[HikariCP-3.4.5.jar!/:na]
app_1            | 	at com.zaxxer.hikari.HikariDataSource.getConnection(HikariDataSource.java:112) ~[HikariCP-3.4.5.jar!/:na]
app_1            | 	at org.springframework.jdbc.datasource.DataSourceUtils.fetchConnection(DataSourceUtils.java:158) ~[spring-jdbc-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.jdbc.datasource.DataSourceUtils.doGetConnection(DataSourceUtils.java:116) ~[spring-jdbc-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.jdbc.datasource.DataSourceUtils.getConnection(DataSourceUtils.java:79) ~[spring-jdbc-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.jdbc.core.JdbcTemplate.execute(JdbcTemplate.java:324) ~[spring-jdbc-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.boot.jdbc.EmbeddedDatabaseConnection.isEmbedded(EmbeddedDatabaseConnection.java:133) ~[spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
app_1            | 	at org.springframework.boot.autoconfigure.orm.jpa.HibernateDefaultDdlAutoProvider.getDefaultDdlAuto(HibernateDefaultDdlAutoProvider.java:42) ~[spring-boot-autoconfigure-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
app_1            | 	at org.springframework.boot.autoconfigure.orm.jpa.HibernateJpaConfiguration.lambda$getVendorProperties$1(HibernateJpaConfiguration.java:130) ~[spring-boot-autoconfigure-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
app_1            | 	at org.springframework.boot.autoconfigure.orm.jpa.HibernateSettings.getDdlAuto(HibernateSettings.java:41) ~[spring-boot-autoconfigure-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
app_1            | 	at org.springframework.boot.autoconfigure.orm.jpa.HibernateProperties.determineDdlAuto(HibernateProperties.java:136) ~[spring-boot-autoconfigure-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
app_1            | 	at org.springframework.boot.autoconfigure.orm.jpa.HibernateProperties.getAdditionalProperties(HibernateProperties.java:102) ~[spring-boot-autoconfigure-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
app_1            | 	at org.springframework.boot.autoconfigure.orm.jpa.HibernateProperties.determineHibernateProperties(HibernateProperties.java:94) ~[spring-boot-autoconfigure-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
app_1            | 	at org.springframework.boot.autoconfigure.orm.jpa.HibernateJpaConfiguration.getVendorProperties(HibernateJpaConfiguration.java:132) ~[spring-boot-autoconfigure-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
app_1            | 	at org.springframework.boot.autoconfigure.orm.jpa.JpaBaseConfiguration.entityManagerFactory(JpaBaseConfiguration.java:133) ~[spring-boot-autoconfigure-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
app_1            | 	at java.base/jdk.internal.reflect.NativeMethodAccessorImpl.invoke0(Native Method) ~[na:na]
app_1            | 	at java.base/jdk.internal.reflect.NativeMethodAccessorImpl.invoke(Unknown Source) ~[na:na]
app_1            | 	at java.base/jdk.internal.reflect.DelegatingMethodAccessorImpl.invoke(Unknown Source) ~[na:na]
app_1            | 	at java.base/java.lang.reflect.Method.invoke(Unknown Source) ~[na:na]
app_1            | 	at org.springframework.beans.factory.support.SimpleInstantiationStrategy.instantiate(SimpleInstantiationStrategy.java:154) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.beans.factory.support.ConstructorResolver.instantiate(ConstructorResolver.java:650) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.beans.factory.support.ConstructorResolver.instantiateUsingFactoryMethod(ConstructorResolver.java:635) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory.instantiateUsingFactoryMethod(AbstractAutowireCapableBeanFactory.java:1336) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory.createBeanInstance(AbstractAutowireCapableBeanFactory.java:1176) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory.doCreateBean(AbstractAutowireCapableBeanFactory.java:556) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory.createBean(AbstractAutowireCapableBeanFactory.java:516) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.beans.factory.support.AbstractBeanFactory.lambda$doGetBean$0(AbstractBeanFactory.java:324) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.beans.factory.support.DefaultSingletonBeanRegistry.getSingleton(DefaultSingletonBeanRegistry.java:234) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.beans.factory.support.AbstractBeanFactory.doGetBean(AbstractBeanFactory.java:322) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.beans.factory.support.AbstractBeanFactory.getBean(AbstractBeanFactory.java:202) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.context.support.AbstractApplicationContext.getBean(AbstractApplicationContext.java:1109) ~[spring-context-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.context.support.AbstractApplicationContext.finishBeanFactoryInitialization(AbstractApplicationContext.java:869) ~[spring-context-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.context.support.AbstractApplicationContext.refresh(AbstractApplicationContext.java:551) ~[spring-context-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.boot.SpringApplication.refresh(SpringApplication.java:758) ~[spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
app_1            | 	at org.springframework.boot.SpringApplication.refresh(SpringApplication.java:750) ~[spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
app_1            | 	at org.springframework.boot.SpringApplication.refreshContext(SpringApplication.java:397) ~[spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
app_1            | 	at org.springframework.boot.SpringApplication.run(SpringApplication.java:315) ~[spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
app_1            | 	at org.springframework.boot.SpringApplication.run(SpringApplication.java:1237) ~[spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
app_1            | 	at org.springframework.boot.SpringApplication.run(SpringApplication.java:1226) ~[spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
app_1            | 	at name.ealen.SpringBatchApplication.main(SpringBatchApplication.java:13) ~[classes!/:0.1.0-SNAPSNOT]
app_1            | 	at java.base/jdk.internal.reflect.NativeMethodAccessorImpl.invoke0(Native Method) ~[na:na]
app_1            | 	at java.base/jdk.internal.reflect.NativeMethodAccessorImpl.invoke(Unknown Source) ~[na:na]
app_1            | 	at java.base/jdk.internal.reflect.DelegatingMethodAccessorImpl.invoke(Unknown Source) ~[na:na]
app_1            | 	at java.base/java.lang.reflect.Method.invoke(Unknown Source) ~[na:na]
app_1            | 	at org.springframework.boot.loader.MainMethodRunner.run(MainMethodRunner.java:49) ~[app.jar:0.1.0-SNAPSNOT]
app_1            | 	at org.springframework.boot.loader.Launcher.launch(Launcher.java:107) ~[app.jar:0.1.0-SNAPSNOT]
app_1            | 	at org.springframework.boot.loader.Launcher.launch(Launcher.java:58) ~[app.jar:0.1.0-SNAPSNOT]
app_1            | 	at org.springframework.boot.loader.JarLauncher.main(JarLauncher.java:88) ~[app.jar:0.1.0-SNAPSNOT]
app_1            | Caused by: java.net.ConnectException: Connection refused (Connection refused)
app_1            | 	at java.base/java.net.PlainSocketImpl.socketConnect(Native Method) ~[na:na]
app_1            | 	at java.base/java.net.AbstractPlainSocketImpl.doConnect(Unknown Source) ~[na:na]
app_1            | 	at java.base/java.net.AbstractPlainSocketImpl.connectToAddress(Unknown Source) ~[na:na]
app_1            | 	at java.base/java.net.AbstractPlainSocketImpl.connect(Unknown Source) ~[na:na]
app_1            | 	at java.base/java.net.SocksSocketImpl.connect(Unknown Source) ~[na:na]
app_1            | 	at java.base/java.net.Socket.connect(Unknown Source) ~[na:na]
app_1            | 	at com.mysql.jdbc.StandardSocketFactory.connect(StandardSocketFactory.java:211) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	at com.mysql.jdbc.MysqlIO.<init>(MysqlIO.java:299) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	... 65 common frames omitted
app_1            | 
app_1            | 2026-04-18 02:01:25.815  INFO 1 --- [      Data-Job1] o.hibernate.jpa.internal.util.LogHelper  : HHH000204: Processing PersistenceUnitInfo [name: default]
app_1            | 2026-04-18 02:01:26.421  INFO 1 --- [      Data-Job1] org.hibernate.Version                    : HHH000412: Hibernate ORM core version 5.4.21.Final
app_1            | 2026-04-18 02:01:26.548  INFO 1 --- [           main] com.zaxxer.hikari.HikariDataSource       : HikariPool-1 - Starting...
app_1            | Sat Apr 18 02:01:26 GMT 2026 WARN: Establishing SSL connection without server's identity verification is not recommended. According to MySQL 5.5.45+, 5.6.26+ and 5.7.6+ requirements SSL connection must be established by default if explicit option isn't set. For compliance with existing applications not using SSL the verifyServerCertificate property is set to 'false'. You need either to explicitly disable SSL by setting useSSL=false, or set useSSL=true and provide truststore for server certificate verification.
app_1            | 2026-04-18 02:01:27.203  INFO 1 --- [      Data-Job1] o.hibernate.annotations.common.Version   : HCANN000001: Hibernate Commons Annotations {5.1.0.Final}
app_1            | 2026-04-18 02:01:28.141 ERROR 1 --- [           main] com.zaxxer.hikari.pool.HikariPool        : HikariPool-1 - Exception during pool initialization.
app_1            | 
app_1            | com.mysql.jdbc.exceptions.jdbc4.CommunicationsException: Communications link failure
app_1            | 
app_1            | The last packet successfully received from the server was 573 milliseconds ago.  The last packet sent successfully to the server was 529 milliseconds ago.
app_1            | 	at java.base/jdk.internal.reflect.NativeConstructorAccessorImpl.newInstance0(Native Method) ~[na:na]
app_1            | 	at java.base/jdk.internal.reflect.NativeConstructorAccessorImpl.newInstance(Unknown Source) ~[na:na]
app_1            | 	at java.base/jdk.internal.reflect.DelegatingConstructorAccessorImpl.newInstance(Unknown Source) ~[na:na]
app_1            | 	at java.base/java.lang.reflect.Constructor.newInstance(Unknown Source) ~[na:na]
app_1            | 	at com.mysql.jdbc.Util.handleNewInstance(Util.java:403) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	at com.mysql.jdbc.SQLError.createCommunicationsException(SQLError.java:990) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	at com.mysql.jdbc.ExportControlled.transformSocketToSSLSocket(ExportControlled.java:202) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	at com.mysql.jdbc.MysqlIO.negotiateSSLConnection(MysqlIO.java:4869) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	at com.mysql.jdbc.MysqlIO.proceedHandshakeWithPluggableAuthentication(MysqlIO.java:1656) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	at com.mysql.jdbc.MysqlIO.doHandshake(MysqlIO.java:1217) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	at com.mysql.jdbc.ConnectionImpl.coreConnect(ConnectionImpl.java:2189) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	at com.mysql.jdbc.ConnectionImpl.connectOneTryOnly(ConnectionImpl.java:2220) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	at com.mysql.jdbc.ConnectionImpl.createNewIO(ConnectionImpl.java:2015) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	at com.mysql.jdbc.ConnectionImpl.<init>(ConnectionImpl.java:768) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	at com.mysql.jdbc.JDBC4Connection.<init>(JDBC4Connection.java:47) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	at java.base/jdk.internal.reflect.NativeConstructorAccessorImpl.newInstance0(Native Method) ~[na:na]
app_1            | 	at java.base/jdk.internal.reflect.NativeConstructorAccessorImpl.newInstance(Unknown Source) ~[na:na]
app_1            | 	at java.base/jdk.internal.reflect.DelegatingConstructorAccessorImpl.newInstance(Unknown Source) ~[na:na]
app_1            | 	at java.base/java.lang.reflect.Constructor.newInstance(Unknown Source) ~[na:na]
app_1            | 	at com.mysql.jdbc.Util.handleNewInstance(Util.java:403) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	at com.mysql.jdbc.ConnectionImpl.getInstance(ConnectionImpl.java:385) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	at com.mysql.jdbc.NonRegisteringDriver.connect(NonRegisteringDriver.java:323) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	at com.zaxxer.hikari.util.DriverDataSource.getConnection(DriverDataSource.java:138) ~[HikariCP-3.4.5.jar!/:na]
app_1            | 	at com.zaxxer.hikari.pool.PoolBase.newConnection(PoolBase.java:358) ~[HikariCP-3.4.5.jar!/:na]
app_1            | 	at com.zaxxer.hikari.pool.PoolBase.newPoolEntry(PoolBase.java:206) ~[HikariCP-3.4.5.jar!/:na]
app_1            | 	at com.zaxxer.hikari.pool.HikariPool.createPoolEntry(HikariPool.java:477) ~[HikariCP-3.4.5.jar!/:na]
app_1            | 	at com.zaxxer.hikari.pool.HikariPool.checkFailFast(HikariPool.java:560) ~[HikariCP-3.4.5.jar!/:na]
app_1            | 	at com.zaxxer.hikari.pool.HikariPool.<init>(HikariPool.java:115) ~[HikariCP-3.4.5.jar!/:na]
app_1            | 	at com.zaxxer.hikari.HikariDataSource.getConnection(HikariDataSource.java:112) ~[HikariCP-3.4.5.jar!/:na]
app_1            | 	at org.springframework.jdbc.datasource.DataSourceUtils.fetchConnection(DataSourceUtils.java:158) ~[spring-jdbc-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.jdbc.datasource.DataSourceUtils.doGetConnection(DataSourceUtils.java:116) ~[spring-jdbc-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.jdbc.datasource.DataSourceUtils.getConnection(DataSourceUtils.java:79) ~[spring-jdbc-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.jdbc.core.JdbcTemplate.execute(JdbcTemplate.java:324) ~[spring-jdbc-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.boot.jdbc.EmbeddedDatabaseConnection.isEmbedded(EmbeddedDatabaseConnection.java:133) ~[spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
app_1            | 	at org.springframework.boot.jdbc.AbstractDataSourceInitializer.isEnabled(AbstractDataSourceInitializer.java:75) ~[spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
app_1            | 	at org.springframework.boot.jdbc.AbstractDataSourceInitializer.initialize(AbstractDataSourceInitializer.java:55) ~[spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
app_1            | 	at java.base/jdk.internal.reflect.NativeMethodAccessorImpl.invoke0(Native Method) ~[na:na]
app_1            | 	at java.base/jdk.internal.reflect.NativeMethodAccessorImpl.invoke(Unknown Source) ~[na:na]
app_1            | 	at java.base/jdk.internal.reflect.DelegatingMethodAccessorImpl.invoke(Unknown Source) ~[na:na]
app_1            | 	at java.base/java.lang.reflect.Method.invoke(Unknown Source) ~[na:na]
app_1            | 	at org.springframework.beans.factory.annotation.InitDestroyAnnotationBeanPostProcessor$LifecycleElement.invoke(InitDestroyAnnotationBeanPostProcessor.java:389) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.beans.factory.annotation.InitDestroyAnnotationBeanPostProcessor$LifecycleMetadata.invokeInitMethods(InitDestroyAnnotationBeanPostProcessor.java:333) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.beans.factory.annotation.InitDestroyAnnotationBeanPostProcessor.postProcessBeforeInitialization(InitDestroyAnnotationBeanPostProcessor.java:157) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory.applyBeanPostProcessorsBeforeInitialization(AbstractAutowireCapableBeanFactory.java:415) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory.initializeBean(AbstractAutowireCapableBeanFactory.java:1786) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory.doCreateBean(AbstractAutowireCapableBeanFactory.java:594) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory.createBean(AbstractAutowireCapableBeanFactory.java:516) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.beans.factory.support.AbstractBeanFactory.lambda$doGetBean$0(AbstractBeanFactory.java:324) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.beans.factory.support.DefaultSingletonBeanRegistry.getSingleton(DefaultSingletonBeanRegistry.java:234) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.beans.factory.support.AbstractBeanFactory.doGetBean(AbstractBeanFactory.java:322) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.beans.factory.support.AbstractBeanFactory.getBean(AbstractBeanFactory.java:202) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.beans.factory.support.DefaultListableBeanFactory.preInstantiateSingletons(DefaultListableBeanFactory.java:897) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.context.support.AbstractApplicationContext.finishBeanFactoryInitialization(AbstractApplicationContext.java:879) ~[spring-context-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.context.support.AbstractApplicationContext.refresh(AbstractApplicationContext.java:551) ~[spring-context-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.boot.SpringApplication.refresh(SpringApplication.java:758) ~[spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
app_1            | 	at org.springframework.boot.SpringApplication.refresh(SpringApplication.java:750) ~[spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
app_1            | 	at org.springframework.boot.SpringApplication.refreshContext(SpringApplication.java:397) ~[spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
app_1            | 	at org.springframework.boot.SpringApplication.run(SpringApplication.java:315) ~[spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
app_1            | 	at org.springframework.boot.SpringApplication.run(SpringApplication.java:1237) ~[spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
app_1            | 	at org.springframework.boot.SpringApplication.run(SpringApplication.java:1226) ~[spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
app_1            | 	at name.ealen.SpringBatchApplication.main(SpringBatchApplication.java:13) ~[classes!/:0.1.0-SNAPSNOT]
app_1            | 	at java.base/jdk.internal.reflect.NativeMethodAccessorImpl.invoke0(Native Method) ~[na:na]
app_1            | 	at java.base/jdk.internal.reflect.NativeMethodAccessorImpl.invoke(Unknown Source) ~[na:na]
app_1            | 	at java.base/jdk.internal.reflect.DelegatingMethodAccessorImpl.invoke(Unknown Source) ~[na:na]
app_1            | 	at java.base/java.lang.reflect.Method.invoke(Unknown Source) ~[na:na]
app_1            | 	at org.springframework.boot.loader.MainMethodRunner.run(MainMethodRunner.java:49) ~[app.jar:0.1.0-SNAPSNOT]
app_1            | 	at org.springframework.boot.loader.Launcher.launch(Launcher.java:107) ~[app.jar:0.1.0-SNAPSNOT]
app_1            | 	at org.springframework.boot.loader.Launcher.launch(Launcher.java:58) ~[app.jar:0.1.0-SNAPSNOT]
app_1            | 	at org.springframework.boot.loader.JarLauncher.main(JarLauncher.java:88) ~[app.jar:0.1.0-SNAPSNOT]
app_1            | Caused by: javax.net.ssl.SSLHandshakeException: Received fatal alert: handshake_failure
app_1            | 	at java.base/sun.security.ssl.Alert.createSSLException(Unknown Source) ~[na:na]
app_1            | 	at java.base/sun.security.ssl.Alert.createSSLException(Unknown Source) ~[na:na]
app_1            | 	at java.base/sun.security.ssl.TransportContext.fatal(Unknown Source) ~[na:na]
app_1            | 	at java.base/sun.security.ssl.Alert$AlertConsumer.consume(Unknown Source) ~[na:na]
app_1            | 	at java.base/sun.security.ssl.TransportContext.dispatch(Unknown Source) ~[na:na]
app_1            | 	at java.base/sun.security.ssl.SSLTransport.decode(Unknown Source) ~[na:na]
app_1            | 	at java.base/sun.security.ssl.SSLSocketImpl.decode(Unknown Source) ~[na:na]
app_1            | 	at java.base/sun.security.ssl.SSLSocketImpl.readHandshakeRecord(Unknown Source) ~[na:na]
app_1            | 	at java.base/sun.security.ssl.SSLSocketImpl.startHandshake(Unknown Source) ~[na:na]
app_1            | 	at java.base/sun.security.ssl.SSLSocketImpl.startHandshake(Unknown Source) ~[na:na]
app_1            | 	at com.mysql.jdbc.ExportControlled.transformSocketToSSLSocket(ExportControlled.java:187) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	... 62 common frames omitted
app_1            | 
app_1            | 2026-04-18 02:01:28.145  INFO 1 --- [      Data-Job1] com.zaxxer.hikari.HikariDataSource       : HikariPool-1 - Starting...
app_1            | Sat Apr 18 02:01:28 GMT 2026 WARN: Establishing SSL connection without server's identity verification is not recommended. According to MySQL 5.5.45+, 5.6.26+ and 5.7.6+ requirements SSL connection must be established by default if explicit option isn't set. For compliance with existing applications not using SSL the verifyServerCertificate property is set to 'false'. You need either to explicitly disable SSL by setting useSSL=false, or set useSSL=true and provide truststore for server certificate verification.
app_1            | 2026-04-18 02:01:28.320  WARN 1 --- [           main] o.s.b.a.batch.JpaBatchConfigurer         : JPA does not support custom isolation levels, so locks may not be taken when launching Jobs
app_1            | 2026-04-18 02:01:29.175 ERROR 1 --- [      Data-Job1] com.zaxxer.hikari.pool.HikariPool        : HikariPool-1 - Exception during pool initialization.
app_1            | 
app_1            | com.mysql.jdbc.exceptions.jdbc4.CommunicationsException: Communications link failure
app_1            | 
app_1            | The last packet successfully received from the server was 19 milliseconds ago.  The last packet sent successfully to the server was 12 milliseconds ago.
app_1            | 	at java.base/jdk.internal.reflect.NativeConstructorAccessorImpl.newInstance0(Native Method) ~[na:na]
app_1            | 	at java.base/jdk.internal.reflect.NativeConstructorAccessorImpl.newInstance(Unknown Source) ~[na:na]
app_1            | 	at java.base/jdk.internal.reflect.DelegatingConstructorAccessorImpl.newInstance(Unknown Source) ~[na:na]
app_1            | 	at java.base/java.lang.reflect.Constructor.newInstance(Unknown Source) ~[na:na]
app_1            | 	at com.mysql.jdbc.Util.handleNewInstance(Util.java:403) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	at com.mysql.jdbc.SQLError.createCommunicationsException(SQLError.java:990) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	at com.mysql.jdbc.ExportControlled.transformSocketToSSLSocket(ExportControlled.java:202) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	at com.mysql.jdbc.MysqlIO.negotiateSSLConnection(MysqlIO.java:4869) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	at com.mysql.jdbc.MysqlIO.proceedHandshakeWithPluggableAuthentication(MysqlIO.java:1656) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	at com.mysql.jdbc.MysqlIO.doHandshake(MysqlIO.java:1217) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	at com.mysql.jdbc.ConnectionImpl.coreConnect(ConnectionImpl.java:2189) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	at com.mysql.jdbc.ConnectionImpl.connectOneTryOnly(ConnectionImpl.java:2220) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	at com.mysql.jdbc.ConnectionImpl.createNewIO(ConnectionImpl.java:2015) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	at com.mysql.jdbc.ConnectionImpl.<init>(ConnectionImpl.java:768) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	at com.mysql.jdbc.JDBC4Connection.<init>(JDBC4Connection.java:47) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	at java.base/jdk.internal.reflect.NativeConstructorAccessorImpl.newInstance0(Native Method) ~[na:na]
app_1            | 	at java.base/jdk.internal.reflect.NativeConstructorAccessorImpl.newInstance(Unknown Source) ~[na:na]
app_1            | 	at java.base/jdk.internal.reflect.DelegatingConstructorAccessorImpl.newInstance(Unknown Source) ~[na:na]
app_1            | 	at java.base/java.lang.reflect.Constructor.newInstance(Unknown Source) ~[na:na]
app_1            | 	at com.mysql.jdbc.Util.handleNewInstance(Util.java:403) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	at com.mysql.jdbc.ConnectionImpl.getInstance(ConnectionImpl.java:385) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	at com.mysql.jdbc.NonRegisteringDriver.connect(NonRegisteringDriver.java:323) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	at com.zaxxer.hikari.util.DriverDataSource.getConnection(DriverDataSource.java:138) ~[HikariCP-3.4.5.jar!/:na]
app_1            | 	at com.zaxxer.hikari.pool.PoolBase.newConnection(PoolBase.java:358) ~[HikariCP-3.4.5.jar!/:na]
app_1            | 	at com.zaxxer.hikari.pool.PoolBase.newPoolEntry(PoolBase.java:206) ~[HikariCP-3.4.5.jar!/:na]
app_1            | 	at com.zaxxer.hikari.pool.HikariPool.createPoolEntry(HikariPool.java:477) ~[HikariCP-3.4.5.jar!/:na]
app_1            | 	at com.zaxxer.hikari.pool.HikariPool.checkFailFast(HikariPool.java:560) ~[HikariCP-3.4.5.jar!/:na]
app_1            | 	at com.zaxxer.hikari.pool.HikariPool.<init>(HikariPool.java:115) ~[HikariCP-3.4.5.jar!/:na]
app_1            | 	at com.zaxxer.hikari.HikariDataSource.getConnection(HikariDataSource.java:112) ~[HikariCP-3.4.5.jar!/:na]
app_1            | 	at org.hibernate.engine.jdbc.connections.internal.DatasourceConnectionProviderImpl.getConnection(DatasourceConnectionProviderImpl.java:122) ~[hibernate-core-5.4.21.Final.jar!/:5.4.21.Final]
app_1            | 	at org.hibernate.engine.jdbc.env.internal.JdbcEnvironmentInitiator$ConnectionProviderJdbcConnectionAccess.obtainConnection(JdbcEnvironmentInitiator.java:180) ~[hibernate-core-5.4.21.Final.jar!/:5.4.21.Final]
app_1            | 	at org.hibernate.engine.jdbc.env.internal.JdbcEnvironmentInitiator.initiateService(JdbcEnvironmentInitiator.java:68) ~[hibernate-core-5.4.21.Final.jar!/:5.4.21.Final]
app_1            | 	at org.hibernate.engine.jdbc.env.internal.JdbcEnvironmentInitiator.initiateService(JdbcEnvironmentInitiator.java:35) ~[hibernate-core-5.4.21.Final.jar!/:5.4.21.Final]
app_1            | 	at org.hibernate.boot.registry.internal.StandardServiceRegistryImpl.initiateService(StandardServiceRegistryImpl.java:101) ~[hibernate-core-5.4.21.Final.jar!/:5.4.21.Final]
app_1            | 	at org.hibernate.service.internal.AbstractServiceRegistryImpl.createService(AbstractServiceRegistryImpl.java:263) ~[hibernate-core-5.4.21.Final.jar!/:5.4.21.Final]
app_1            | 	at org.hibernate.service.internal.AbstractServiceRegistryImpl.initializeService(AbstractServiceRegistryImpl.java:237) ~[hibernate-core-5.4.21.Final.jar!/:5.4.21.Final]
app_1            | 	at org.hibernate.service.internal.AbstractServiceRegistryImpl.getService(AbstractServiceRegistryImpl.java:214) ~[hibernate-core-5.4.21.Final.jar!/:5.4.21.Final]
app_1            | 	at org.hibernate.id.factory.internal.DefaultIdentifierGeneratorFactory.injectServices(DefaultIdentifierGeneratorFactory.java:152) ~[hibernate-core-5.4.21.Final.jar!/:5.4.21.Final]
app_1            | 	at org.hibernate.service.internal.AbstractServiceRegistryImpl.injectDependencies(AbstractServiceRegistryImpl.java:286) ~[hibernate-core-5.4.21.Final.jar!/:5.4.21.Final]
app_1            | 	at org.hibernate.service.internal.AbstractServiceRegistryImpl.initializeService(AbstractServiceRegistryImpl.java:243) ~[hibernate-core-5.4.21.Final.jar!/:5.4.21.Final]
app_1            | 	at org.hibernate.service.internal.AbstractServiceRegistryImpl.getService(AbstractServiceRegistryImpl.java:214) ~[hibernate-core-5.4.21.Final.jar!/:5.4.21.Final]
app_1            | 	at org.hibernate.boot.internal.InFlightMetadataCollectorImpl.<init>(InFlightMetadataCollectorImpl.java:176) ~[hibernate-core-5.4.21.Final.jar!/:5.4.21.Final]
app_1            | 	at org.hibernate.boot.model.process.spi.MetadataBuildingProcess.complete(MetadataBuildingProcess.java:118) ~[hibernate-core-5.4.21.Final.jar!/:5.4.21.Final]
app_1            | 	at org.hibernate.jpa.boot.internal.EntityManagerFactoryBuilderImpl.metadata(EntityManagerFactoryBuilderImpl.java:1224) ~[hibernate-core-5.4.21.Final.jar!/:5.4.21.Final]
app_1            | 	at org.hibernate.jpa.boot.internal.EntityManagerFactoryBuilderImpl.build(EntityManagerFactoryBuilderImpl.java:1255) ~[hibernate-core-5.4.21.Final.jar!/:5.4.21.Final]
app_1            | 	at org.springframework.orm.jpa.vendor.SpringHibernateJpaPersistenceProvider.createContainerEntityManagerFactory(SpringHibernateJpaPersistenceProvider.java:58) ~[spring-orm-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.orm.jpa.LocalContainerEntityManagerFactoryBean.createNativeEntityManagerFactory(LocalContainerEntityManagerFactoryBean.java:365) ~[spring-orm-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.orm.jpa.AbstractEntityManagerFactoryBean.buildNativeEntityManagerFactory(AbstractEntityManagerFactoryBean.java:391) ~[spring-orm-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at java.base/java.util.concurrent.FutureTask.run(Unknown Source) ~[na:na]
app_1            | 	at java.base/java.util.concurrent.ThreadPoolExecutor.runWorker(Unknown Source) ~[na:na]
app_1            | 	at java.base/java.util.concurrent.ThreadPoolExecutor$Worker.run(Unknown Source) ~[na:na]
app_1            | 	at java.base/java.lang.Thread.run(Unknown Source) ~[na:na]
app_1            | Caused by: javax.net.ssl.SSLHandshakeException: Received fatal alert: handshake_failure
app_1            | 	at java.base/sun.security.ssl.Alert.createSSLException(Unknown Source) ~[na:na]
app_1            | 	at java.base/sun.security.ssl.Alert.createSSLException(Unknown Source) ~[na:na]
app_1            | 	at java.base/sun.security.ssl.TransportContext.fatal(Unknown Source) ~[na:na]
app_1            | 	at java.base/sun.security.ssl.Alert$AlertConsumer.consume(Unknown Source) ~[na:na]
app_1            | 	at java.base/sun.security.ssl.TransportContext.dispatch(Unknown Source) ~[na:na]
app_1            | 	at java.base/sun.security.ssl.SSLTransport.decode(Unknown Source) ~[na:na]
app_1            | 	at java.base/sun.security.ssl.SSLSocketImpl.decode(Unknown Source) ~[na:na]
app_1            | 	at java.base/sun.security.ssl.SSLSocketImpl.readHandshakeRecord(Unknown Source) ~[na:na]
app_1            | 	at java.base/sun.security.ssl.SSLSocketImpl.startHandshake(Unknown Source) ~[na:na]
app_1            | 	at java.base/sun.security.ssl.SSLSocketImpl.startHandshake(Unknown Source) ~[na:na]
app_1            | 	at com.mysql.jdbc.ExportControlled.transformSocketToSSLSocket(ExportControlled.java:187) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	... 45 common frames omitted
app_1            | 
app_1            | 2026-04-18 02:01:29.185  INFO 1 --- [           main] com.zaxxer.hikari.HikariDataSource       : HikariPool-1 - Starting...
app_1            | Sat Apr 18 02:01:29 GMT 2026 WARN: Establishing SSL connection without server's identity verification is not recommended. According to MySQL 5.5.45+, 5.6.26+ and 5.7.6+ requirements SSL connection must be established by default if explicit option isn't set. For compliance with existing applications not using SSL the verifyServerCertificate property is set to 'false'. You need either to explicitly disable SSL by setting useSSL=false, or set useSSL=true and provide truststore for server certificate verification.
app_1            | 2026-04-18 02:01:29.190  WARN 1 --- [      Data-Job1] o.h.e.j.e.i.JdbcEnvironmentInitiator     : HHH000342: Could not obtain connection to query metadata : Communications link failure
app_1            | 
app_1            | The last packet successfully received from the server was 19 milliseconds ago.  The last packet sent successfully to the server was 12 milliseconds ago.
app_1            | 2026-04-18 02:01:30.202 ERROR 1 --- [           main] com.zaxxer.hikari.pool.HikariPool        : HikariPool-1 - Exception during pool initialization.
app_1            | 
app_1            | com.mysql.jdbc.exceptions.jdbc4.CommunicationsException: Communications link failure
app_1            | 
app_1            | The last packet successfully received from the server was 6 milliseconds ago.  The last packet sent successfully to the server was 6 milliseconds ago.
app_1            | 	at java.base/jdk.internal.reflect.NativeConstructorAccessorImpl.newInstance0(Native Method) ~[na:na]
app_1            | 	at java.base/jdk.internal.reflect.NativeConstructorAccessorImpl.newInstance(Unknown Source) ~[na:na]
app_1            | 	at java.base/jdk.internal.reflect.DelegatingConstructorAccessorImpl.newInstance(Unknown Source) ~[na:na]
app_1            | 	at java.base/java.lang.reflect.Constructor.newInstance(Unknown Source) ~[na:na]
app_1            | 	at com.mysql.jdbc.Util.handleNewInstance(Util.java:403) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	at com.mysql.jdbc.SQLError.createCommunicationsException(SQLError.java:990) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	at com.mysql.jdbc.ExportControlled.transformSocketToSSLSocket(ExportControlled.java:202) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	at com.mysql.jdbc.MysqlIO.negotiateSSLConnection(MysqlIO.java:4869) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	at com.mysql.jdbc.MysqlIO.proceedHandshakeWithPluggableAuthentication(MysqlIO.java:1656) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	at com.mysql.jdbc.MysqlIO.doHandshake(MysqlIO.java:1217) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	at com.mysql.jdbc.ConnectionImpl.coreConnect(ConnectionImpl.java:2189) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	at com.mysql.jdbc.ConnectionImpl.connectOneTryOnly(ConnectionImpl.java:2220) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	at com.mysql.jdbc.ConnectionImpl.createNewIO(ConnectionImpl.java:2015) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	at com.mysql.jdbc.ConnectionImpl.<init>(ConnectionImpl.java:768) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	at com.mysql.jdbc.JDBC4Connection.<init>(JDBC4Connection.java:47) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	at java.base/jdk.internal.reflect.NativeConstructorAccessorImpl.newInstance0(Native Method) ~[na:na]
app_1            | 	at java.base/jdk.internal.reflect.NativeConstructorAccessorImpl.newInstance(Unknown Source) ~[na:na]
app_1            | 	at java.base/jdk.internal.reflect.DelegatingConstructorAccessorImpl.newInstance(Unknown Source) ~[na:na]
app_1            | 	at java.base/java.lang.reflect.Constructor.newInstance(Unknown Source) ~[na:na]
app_1            | 	at com.mysql.jdbc.Util.handleNewInstance(Util.java:403) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	at com.mysql.jdbc.ConnectionImpl.getInstance(ConnectionImpl.java:385) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	at com.mysql.jdbc.NonRegisteringDriver.connect(NonRegisteringDriver.java:323) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	at com.zaxxer.hikari.util.DriverDataSource.getConnection(DriverDataSource.java:138) ~[HikariCP-3.4.5.jar!/:na]
app_1            | 	at com.zaxxer.hikari.pool.PoolBase.newConnection(PoolBase.java:358) ~[HikariCP-3.4.5.jar!/:na]
app_1            | 	at com.zaxxer.hikari.pool.PoolBase.newPoolEntry(PoolBase.java:206) ~[HikariCP-3.4.5.jar!/:na]
app_1            | 	at com.zaxxer.hikari.pool.HikariPool.createPoolEntry(HikariPool.java:477) ~[HikariCP-3.4.5.jar!/:na]
app_1            | 	at com.zaxxer.hikari.pool.HikariPool.checkFailFast(HikariPool.java:560) ~[HikariCP-3.4.5.jar!/:na]
app_1            | 	at com.zaxxer.hikari.pool.HikariPool.<init>(HikariPool.java:115) ~[HikariCP-3.4.5.jar!/:na]
app_1            | 	at com.zaxxer.hikari.HikariDataSource.getConnection(HikariDataSource.java:112) ~[HikariCP-3.4.5.jar!/:na]
app_1            | 	at org.springframework.jdbc.datasource.DataSourceUtils.fetchConnection(DataSourceUtils.java:158) ~[spring-jdbc-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.jdbc.datasource.DataSourceUtils.doGetConnection(DataSourceUtils.java:116) ~[spring-jdbc-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.jdbc.datasource.DataSourceUtils.getConnection(DataSourceUtils.java:79) ~[spring-jdbc-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.jdbc.support.JdbcUtils.extractDatabaseMetaData(JdbcUtils.java:337) ~[spring-jdbc-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.jdbc.support.JdbcUtils.extractDatabaseMetaData(JdbcUtils.java:395) ~[spring-jdbc-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.batch.support.DatabaseType.fromMetaData(DatabaseType.java:102) ~[spring-batch-infrastructure-4.2.4.RELEASE.jar!/:4.2.4.RELEASE]
app_1            | 	at org.springframework.batch.core.repository.support.JobRepositoryFactoryBean.afterPropertiesSet(JobRepositoryFactoryBean.java:183) ~[spring-batch-core-4.2.4.RELEASE.jar!/:4.2.4.RELEASE]
app_1            | 	at org.springframework.boot.autoconfigure.batch.BasicBatchConfigurer.createJobRepository(BasicBatchConfigurer.java:129) ~[spring-boot-autoconfigure-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
app_1            | 	at org.springframework.boot.autoconfigure.batch.BasicBatchConfigurer.initialize(BasicBatchConfigurer.java:97) ~[spring-boot-autoconfigure-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
app_1            | 	at java.base/jdk.internal.reflect.NativeMethodAccessorImpl.invoke0(Native Method) ~[na:na]
app_1            | 	at java.base/jdk.internal.reflect.NativeMethodAccessorImpl.invoke(Unknown Source) ~[na:na]
app_1            | 	at java.base/jdk.internal.reflect.DelegatingMethodAccessorImpl.invoke(Unknown Source) ~[na:na]
app_1            | 	at java.base/java.lang.reflect.Method.invoke(Unknown Source) ~[na:na]
app_1            | 	at org.springframework.beans.factory.annotation.InitDestroyAnnotationBeanPostProcessor$LifecycleElement.invoke(InitDestroyAnnotationBeanPostProcessor.java:389) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.beans.factory.annotation.InitDestroyAnnotationBeanPostProcessor$LifecycleMetadata.invokeInitMethods(InitDestroyAnnotationBeanPostProcessor.java:333) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.beans.factory.annotation.InitDestroyAnnotationBeanPostProcessor.postProcessBeforeInitialization(InitDestroyAnnotationBeanPostProcessor.java:157) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory.applyBeanPostProcessorsBeforeInitialization(AbstractAutowireCapableBeanFactory.java:415) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory.initializeBean(AbstractAutowireCapableBeanFactory.java:1786) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory.doCreateBean(AbstractAutowireCapableBeanFactory.java:594) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory.createBean(AbstractAutowireCapableBeanFactory.java:516) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.beans.factory.support.AbstractBeanFactory.lambda$doGetBean$0(AbstractBeanFactory.java:324) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.beans.factory.support.DefaultSingletonBeanRegistry.getSingleton(DefaultSingletonBeanRegistry.java:234) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.beans.factory.support.AbstractBeanFactory.doGetBean(AbstractBeanFactory.java:322) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.beans.factory.support.AbstractBeanFactory.getBean(AbstractBeanFactory.java:202) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.beans.factory.support.DefaultListableBeanFactory.preInstantiateSingletons(DefaultListableBeanFactory.java:897) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.context.support.AbstractApplicationContext.finishBeanFactoryInitialization(AbstractApplicationContext.java:879) ~[spring-context-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.context.support.AbstractApplicationContext.refresh(AbstractApplicationContext.java:551) ~[spring-context-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.boot.SpringApplication.refresh(SpringApplication.java:758) ~[spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
app_1            | 	at org.springframework.boot.SpringApplication.refresh(SpringApplication.java:750) ~[spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
app_1            | 	at org.springframework.boot.SpringApplication.refreshContext(SpringApplication.java:397) ~[spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
app_1            | 	at org.springframework.boot.SpringApplication.run(SpringApplication.java:315) ~[spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
app_1            | 	at org.springframework.boot.SpringApplication.run(SpringApplication.java:1237) ~[spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
app_1            | 	at org.springframework.boot.SpringApplication.run(SpringApplication.java:1226) ~[spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
app_1            | 	at name.ealen.SpringBatchApplication.main(SpringBatchApplication.java:13) ~[classes!/:0.1.0-SNAPSNOT]
app_1            | 	at java.base/jdk.internal.reflect.NativeMethodAccessorImpl.invoke0(Native Method) ~[na:na]
app_1            | 	at java.base/jdk.internal.reflect.NativeMethodAccessorImpl.invoke(Unknown Source) ~[na:na]
app_1            | 	at java.base/jdk.internal.reflect.DelegatingMethodAccessorImpl.invoke(Unknown Source) ~[na:na]
app_1            | 	at java.base/java.lang.reflect.Method.invoke(Unknown Source) ~[na:na]
app_1            | 	at org.springframework.boot.loader.MainMethodRunner.run(MainMethodRunner.java:49) ~[app.jar:0.1.0-SNAPSNOT]
app_1            | 	at org.springframework.boot.loader.Launcher.launch(Launcher.java:107) ~[app.jar:0.1.0-SNAPSNOT]
app_1            | 	at org.springframework.boot.loader.Launcher.launch(Launcher.java:58) ~[app.jar:0.1.0-SNAPSNOT]
app_1            | 	at org.springframework.boot.loader.JarLauncher.main(JarLauncher.java:88) ~[app.jar:0.1.0-SNAPSNOT]
app_1            | Caused by: javax.net.ssl.SSLHandshakeException: Received fatal alert: handshake_failure
app_1            | 	at java.base/sun.security.ssl.Alert.createSSLException(Unknown Source) ~[na:na]
app_1            | 	at java.base/sun.security.ssl.Alert.createSSLException(Unknown Source) ~[na:na]
app_1            | 	at java.base/sun.security.ssl.TransportContext.fatal(Unknown Source) ~[na:na]
app_1            | 	at java.base/sun.security.ssl.Alert$AlertConsumer.consume(Unknown Source) ~[na:na]
app_1            | 	at java.base/sun.security.ssl.TransportContext.dispatch(Unknown Source) ~[na:na]
app_1            | 	at java.base/sun.security.ssl.SSLTransport.decode(Unknown Source) ~[na:na]
app_1            | 	at java.base/sun.security.ssl.SSLSocketImpl.decode(Unknown Source) ~[na:na]
app_1            | 	at java.base/sun.security.ssl.SSLSocketImpl.readHandshakeRecord(Unknown Source) ~[na:na]
app_1            | 	at java.base/sun.security.ssl.SSLSocketImpl.startHandshake(Unknown Source) ~[na:na]
app_1            | 	at java.base/sun.security.ssl.SSLSocketImpl.startHandshake(Unknown Source) ~[na:na]
app_1            | 	at com.mysql.jdbc.ExportControlled.transformSocketToSSLSocket(ExportControlled.java:187) ~[mysql-connector-java-5.1.49.jar!/:5.1.49]
app_1            | 	... 64 common frames omitted
app_1            | 
app_1            | 2026-04-18 02:01:30.205  WARN 1 --- [           main] s.c.a.AnnotationConfigApplicationContext : Exception encountered during context initialization - cancelling refresh attempt: org.springframework.beans.factory.BeanCreationException: Error creating bean with name 'batchConfigurer': Invocation of init method failed; nested exception is java.lang.IllegalStateException: Unable to initialize Spring Batch
app_1            | 2026-04-18 02:01:30.209  WARN 1 --- [           main] o.s.b.f.support.DisposableBeanAdapter    : Destroy method 'close' on bean with name 'getDataReader' threw an exception: org.springframework.batch.item.ItemStreamException: Error while closing item reader
app_1            | 2026-04-18 02:01:30.209  INFO 1 --- [           main] j.LocalContainerEntityManagerFactoryBean : Closing JPA EntityManagerFactory for persistence unit 'default'
app_1            | 2026-04-18 02:01:30.217  WARN 1 --- [           main] o.s.b.f.support.DisposableBeanAdapter    : Invocation of destroy method failed on bean with name 'entityManagerFactory': org.hibernate.service.spi.ServiceException: Unable to create requested service [org.hibernate.engine.jdbc.env.spi.JdbcEnvironment]
app_1            | 2026-04-18 02:01:30.218  INFO 1 --- [           main] o.s.s.concurrent.ThreadPoolTaskExecutor  : Shutting down ExecutorService 'threadPoolTaskExecutor'
app_1            | 2026-04-18 02:01:30.258  INFO 1 --- [           main] ConditionEvaluationReportLoggingListener : 
app_1            | 
app_1            | Error starting ApplicationContext. To display the conditions report re-run your application with 'debug' enabled.
app_1            | 2026-04-18 02:01:30.271 ERROR 1 --- [           main] o.s.boot.SpringApplication               : Application run failed
app_1            | 
app_1            | org.springframework.beans.factory.BeanCreationException: Error creating bean with name 'batchConfigurer': Invocation of init method failed; nested exception is java.lang.IllegalStateException: Unable to initialize Spring Batch
app_1            | 	at org.springframework.beans.factory.annotation.InitDestroyAnnotationBeanPostProcessor.postProcessBeforeInitialization(InitDestroyAnnotationBeanPostProcessor.java:160) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory.applyBeanPostProcessorsBeforeInitialization(AbstractAutowireCapableBeanFactory.java:415) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory.initializeBean(AbstractAutowireCapableBeanFactory.java:1786) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory.doCreateBean(AbstractAutowireCapableBeanFactory.java:594) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory.createBean(AbstractAutowireCapableBeanFactory.java:516) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.beans.factory.support.AbstractBeanFactory.lambda$doGetBean$0(AbstractBeanFactory.java:324) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.beans.factory.support.DefaultSingletonBeanRegistry.getSingleton(DefaultSingletonBeanRegistry.java:234) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.beans.factory.support.AbstractBeanFactory.doGetBean(AbstractBeanFactory.java:322) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.beans.factory.support.AbstractBeanFactory.getBean(AbstractBeanFactory.java:202) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.beans.factory.support.DefaultListableBeanFactory.preInstantiateSingletons(DefaultListableBeanFactory.java:897) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.context.support.AbstractApplicationContext.finishBeanFactoryInitialization(AbstractApplicationContext.java:879) ~[spring-context-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.context.support.AbstractApplicationContext.refresh(AbstractApplicationContext.java:551) ~[spring-context-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.boot.SpringApplication.refresh(SpringApplication.java:758) ~[spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
app_1            | 	at org.springframework.boot.SpringApplication.refresh(SpringApplication.java:750) ~[spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
app_1            | 	at org.springframework.boot.SpringApplication.refreshContext(SpringApplication.java:397) ~[spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
app_1            | 	at org.springframework.boot.SpringApplication.run(SpringApplication.java:315) ~[spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
app_1            | 	at org.springframework.boot.SpringApplication.run(SpringApplication.java:1237) ~[spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
app_1            | 	at org.springframework.boot.SpringApplication.run(SpringApplication.java:1226) ~[spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
app_1            | 	at name.ealen.SpringBatchApplication.main(SpringBatchApplication.java:13) ~[classes!/:0.1.0-SNAPSNOT]
app_1            | 	at java.base/jdk.internal.reflect.NativeMethodAccessorImpl.invoke0(Native Method) ~[na:na]
app_1            | 	at java.base/jdk.internal.reflect.NativeMethodAccessorImpl.invoke(Unknown Source) ~[na:na]
app_1            | 	at java.base/jdk.internal.reflect.DelegatingMethodAccessorImpl.invoke(Unknown Source) ~[na:na]
app_1            | 	at java.base/java.lang.reflect.Method.invoke(Unknown Source) ~[na:na]
app_1            | 	at org.springframework.boot.loader.MainMethodRunner.run(MainMethodRunner.java:49) ~[app.jar:0.1.0-SNAPSNOT]
app_1            | 	at org.springframework.boot.loader.Launcher.launch(Launcher.java:107) ~[app.jar:0.1.0-SNAPSNOT]
app_1            | 	at org.springframework.boot.loader.Launcher.launch(Launcher.java:58) ~[app.jar:0.1.0-SNAPSNOT]
app_1            | 	at org.springframework.boot.loader.JarLauncher.main(JarLauncher.java:88) ~[app.jar:0.1.0-SNAPSNOT]
app_1            | Caused by: java.lang.IllegalStateException: Unable to initialize Spring Batch
app_1            | 	at org.springframework.boot.autoconfigure.batch.BasicBatchConfigurer.initialize(BasicBatchConfigurer.java:102) ~[spring-boot-autoconfigure-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
app_1            | 	at java.base/jdk.internal.reflect.NativeMethodAccessorImpl.invoke0(Native Method) ~[na:na]
app_1            | 	at java.base/jdk.internal.reflect.NativeMethodAccessorImpl.invoke(Unknown Source) ~[na:na]
app_1            | 	at java.base/jdk.internal.reflect.DelegatingMethodAccessorImpl.invoke(Unknown Source) ~[na:na]
app_1            | 	at java.base/java.lang.reflect.Method.invoke(Unknown Source) ~[na:na]
app_1            | 	at org.springframework.beans.factory.annotation.InitDestroyAnnotationBeanPostProcessor$LifecycleElement.invoke(InitDestroyAnnotationBeanPostProcessor.java:389) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.beans.factory.annotation.InitDestroyAnnotationBeanPostProcessor$LifecycleMetadata.invokeInitMethods(InitDestroyAnnotationBeanPostProcessor.java:333) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.beans.factory.annotation.InitDestroyAnnotationBeanPostProcessor.postProcessBeforeInitialization(InitDestroyAnnotationBeanPostProcessor.java:157) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	... 26 common frames omitted
app_1            | Caused by: org.springframework.jdbc.support.MetaDataAccessException: Could not get Connection for extracting meta-data; nested exception is org.springframework.jdbc.CannotGetJdbcConnectionException: Failed to obtain JDBC Connection; nested exception is com.mysql.jdbc.exceptions.jdbc4.CommunicationsException: Communications link failure
app_1            | 
app_1            | The last packet successfully received from the server was 6 milliseconds ago.  The last packet sent successfully to the server was 6 milliseconds ago.
app_1            | 	at org.springframework.jdbc.support.JdbcUtils.extractDatabaseMetaData(JdbcUtils.java:363) ~[spring-jdbc-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.jdbc.support.JdbcUtils.extractDatabaseMetaData(JdbcUtils.java:395) ~[spring-jdbc-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.batch.support.DatabaseType.fromMetaData(DatabaseType.java:102) ~[spring-batch-infrastructure-4.2.4.RELEASE.jar!/:4.2.4.RELEASE]
app_1            | 	at org.springframework.batch.core.repository.support.JobRepositoryFactoryBean.afterPropertiesSet(JobRepositoryFactoryBean.java:183) ~[spring-batch-core-4.2.4.RELEASE.jar!/:4.2.4.RELEASE]
app_1            | 	at org.springframework.boot.autoconfigure.batch.BasicBatchConfigurer.createJobRepository(BasicBatchConfigurer.java:129) ~[spring-boot-autoconfigure-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
app_1            | 	at org.springframework.boot.autoconfigure.batch.BasicBatchConfigurer.initialize(BasicBatchConfigurer.java:97) ~[spring-boot-autoconfigure-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
app_1            | 	... 33 common frames omitted
app_1            | Caused by: org.springframework.jdbc.CannotGetJdbcConnectionException: Failed to obtain JDBC Connection; nested exception is com.mysql.jdbc.exceptions.jdbc4.CommunicationsException: Communications link failure
app_1            | 
app_1            | The last packet successfully received from the server was 6 milliseconds ago.  The last packet sent successfully to the server was 6 milliseconds ago.
app_1            | 	at org.springframework.jdbc.datasource.DataSourceUtils.getConnection(DataSourceUtils.java:82) ~[spring-jdbc-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	at org.springframework.jdbc.support.JdbcUtils.extractDatabaseMetaData(JdbcUtils.java:337) ~[spring-jdbc-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
app_1            | 	... 38 common frames omitted
app_1            | Caused by: com.mysql.jdbc.exceptions.jdbc4.CommunicationsException: Communications link failure
app_1            | 
app_1            | The last packet successfully received from the server was 6 milliseconds ago.  The last packet sent successfully to the server was 6 milliseconds ago.
app_1            | 	at java.base/jdk.internal.reflect.NativeConstructorAccessorImpl.newInstance0(Native Method) ~[na:na]

```


### Cleanup


```sh
docker-compose stop
docker-compose rm -f
```


### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
