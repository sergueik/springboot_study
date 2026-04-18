### Info

Basic docker-compose basic spring batch Demo [spring-batch](https://github.com/EalenXie/springboot-batch) downgraded to MySQL __5.7__.

### Usage

```sh
pushd app
mvn clean package
popd
```
```sh
docker-compose up --build -d
```
```sh
docker-compose ps
```
```text
          Name                        Command                State                              Ports
----------------------------------------------------------------------------------------------------------------------------
basic-spring-batch2_app_1   java -jar /app.jar            Up             0.0.0.0:8080->8080/tcp,:::8080->8080/tcp
mysql-server                docker-entrypoint.sh mysqld   Up (healthy)   0.0.0.0:3306->3306/tcp,:::3306->3306/tcp, 33060/tcp
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
docker-compose logs --no-color app | less
```
```text
Attaching to basic-spring-batch2_app_1
app_1           | waiting for mysql
app_1           | waiting for mysql
app_1           |
app_1           |   .   ____          _            __ _ _
app_1           |  /\\ / ___'_ __ _ _(_)_ __  __ _ \ \ \ \
app_1           | ( ( )\___ | '_ | '_| | '_ \/ _` | \ \ \ \
app_1           |  \\/  ___)| |_)| | | | | || (_| |  ) ) ) )
app_1           |   '  |____| .__|_| |_|_| |_\__, | / / / /
app_1           |  =========|_|==============|___/=/_/_/_/
app_1           |  :: Spring Boot ::        (v2.3.4.RELEASE)
app_1           |
app_1           | 2026-04-18 02:24:32.663  INFO 1 --- [           main] name.ealen.SpringBatchApplication        : Starting SpringBatchApplication v0.1.0-SNAPSNOT on 3a7c99382add with PID 1 (/app.jar started by root in /)
app_1           | 2026-04-18 02:24:32.674  INFO 1 --- [           main] name.ealen.SpringBatchApplication        : No active profile set, falling back to default profiles: default
app_1           | 2026-04-18 02:24:34.445  INFO 1 --- [           main] .s.d.r.c.RepositoryConfigurationDelegate : Bootstrapping Spring Data JPA repositories in DEFERRED mode.
app_1           | 2026-04-18 02:24:34.512  INFO 1 --- [           main] .s.d.r.c.RepositoryConfigurationDelegate : Finished Spring Data repository scanning in 35ms. Found 0 JPA repository interfaces.
app_1           | 2026-04-18 02:24:35.679  INFO 1 --- [           main] o.s.s.concurrent.ThreadPoolTaskExecutor  : Initializing ExecutorService 'threadPoolTaskExecutor'
app_1           | 2026-04-18 02:24:35.712  INFO 1 --- [           main] com.zaxxer.hikari.HikariDataSource       : HikariPool-1 - Starting...
app_1           | 2026-04-18 02:24:36.315  INFO 1 --- [           main] com.zaxxer.hikari.HikariDataSource       : HikariPool-1 - Start completed.
app_1           | 2026-04-18 02:24:36.583  INFO 1 --- [      Data-Job1] o.hibernate.jpa.internal.util.LogHelper  : HHH000204: Processing PersistenceUnitInfo [name: default]
app_1           | 2026-04-18 02:24:36.940  INFO 1 --- [      Data-Job1] org.hibernate.Version                    : HHH000412: Hibernate ORM core version 5.4.21.Final
app_1           | 2026-04-18 02:24:37.194  WARN 1 --- [           main] o.s.b.a.batch.JpaBatchConfigurer         : JPA does not support custom isolation levels, so locks may not be taken when launching Jobs
app_1           | 2026-04-18 02:24:37.212  INFO 1 --- [           main] o.s.b.c.r.s.JobRepositoryFactoryBean     : No database type set, using meta data indicating: MYSQL
app_1           | 2026-04-18 02:24:37.744  INFO 1 --- [      Data-Job1] o.hibernate.annotations.common.Version   : HCANN000001: Hibernate Commons Annotations {5.1.0.Final}
app_1           | 2026-04-18 02:24:37.876  INFO 1 --- [           main] o.s.b.c.l.support.SimpleJobLauncher      : No TaskExecutor has been set, defaulting to synchronous executor.
app_1           | 2026-04-18 02:24:38.231  INFO 1 --- [           main] DeferredRepositoryInitializationListener : Triggering deferred initialization of Spring Data repositories…
app_1           | 2026-04-18 02:24:38.240  INFO 1 --- [           main] DeferredRepositoryInitializationListener : Spring Data repositories initialized!
app_1           | 2026-04-18 02:24:38.265  INFO 1 --- [           main] name.ealen.SpringBatchApplication        : Started SpringBatchApplication in 6.754 seconds (JVM running for 7.776)
app_1           | 2026-04-18 02:24:38.275  INFO 1 --- [           main] o.s.b.a.b.JobLauncherApplicationRunner   : Running default command line with: []
app_1           | 2026-04-18 02:24:38.412  INFO 1 --- [      Data-Job1] org.hibernate.dialect.Dialect            : HHH000400: Using dialect: org.hibernate.dialect.MySQL57Dialect
app_1           | 2026-04-18 02:24:40.312  INFO 1 --- [      Data-Job1] o.h.e.t.j.p.i.JtaPlatformInitiator       : HHH000490: Using JtaPlatform implementation: [org.hibernate.engine.transaction.jta.platform.internal.NoJtaPlatform]
app_1           | 2026-04-18 02:24:40.337  INFO 1 --- [      Data-Job1] j.LocalContainerEntityManagerFactoryBean : Initialized JPA EntityManagerFactory for persistence unit 'default'
app_1           | 2026-04-18 02:24:40.678  INFO 1 --- [           main] o.s.b.c.l.support.SimpleJobLauncher      : Job: [SimpleJob: [name=dataHandleJob]] launched with the following parameters: [{run.id=1}]
app_1           | 2026-04-18 02:24:40.762  INFO 1 --- [           main] name.ealen.listener.JobListener          : job before {run.id=1}
app_1           | 2026-04-18 02:24:40.813  INFO 1 --- [           main] o.s.batch.core.job.SimpleStepHandler     : Executing step: [getData]
app_1           | 2026-04-18 02:24:41.275  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : processor data : Access{id=1, username='ealenxie', shopName='*', categoryName='*', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:17', deleteStatus=false, createTime='null', description='测试数据'}
app_1           | 2026-04-18 02:24:41.278  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : processor data : Access{id=2, username='li.je.3', shopName='*', categoryName='*', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:17', deleteStatus=false, createTime='null', description='null'}
app_1           | 2026-04-18 02:24:41.279  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : processor data : Access{id=3, username='dng.j.2', shopName='*', categoryName='HAIR_CARE', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:17', deleteStatus=false, createTime='null', description='null'}
app_1           | 2026-04-18 02:24:41.280  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : processor data : Access{id=4, username='xu.jan.2', shopName='*', categoryName='*', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:17', deleteStatus=false, createTime='null', description='null'}
app_1           | 2026-04-18 02:24:41.280  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : processor data : Access{id=5, username='huang.ra.1', shopName='*', categoryName='*', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:17', deleteStatus=false, createTime='null', description='null'}
app_1           | 2026-04-18 02:24:41.280  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : processor data : Access{id=6, username='zye.y.4', shopName='*', categoryName='*', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:17', deleteStatus=false, createTime='null', description='null'}
app_1           | 2026-04-18 02:24:41.280  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : processor data : Access{id=7, username='wu.ti.2', shopName='VIP', categoryName='*', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:17', deleteStatus=false, createTime='null', description='null'}
app_1           | 2026-04-18 02:24:41.280  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : processor data : Access{id=8, username='zhou.li.5', shopName='*', categoryName='*', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:18', deleteStatus=false, createTime='null', description='null'}
...
app_1           | 2026-04-18 02:24:42.037  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : processor data : Access{id=406, username='wu.j.5', shopName='XX_Braun', categoryName='*', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:31', deleteStatus=false, createTime='null', description='null'}
app_1           | 2026-04-18 02:24:42.037  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : processor data : Access{id=407, username='wu.j.5', shopName='XX_Gillette', categoryName='*', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:31', deleteStatus=false, createTime='null', description='null'}
app_1           | 2026-04-18 02:24:42.037  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : processor data : Access{id=408, username='wu.j.5', shopName='XX_Global', categoryName='*', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:31', deleteStatus=false, createTime='null', description='null'}
app_1           | 2026-04-18 02:24:42.037  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : processor data : Access{id=409, username='wu.j.5', shopName='XX_Olay', categoryName='*', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:31', deleteStatus=false, createTime='null', description='null'}
app_1           | 2026-04-18 02:24:42.037  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : processor data : Access{id=410, username='wu.j.5', shopName='XX_Oral-B', categoryName='*', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:31', deleteStatus=false, createTime='null', description='null'}
app_1           | 2026-04-18 02:24:42.037  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : processor data : Access{id=411, username='wu.j.5', shopName='XX_P&G', categoryName='*', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:31', deleteStatus=false, createTime='null', description='null'}
app_1           | 2026-04-18 02:24:42.037  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : processor data : Access{id=412, username='wu.j.5', shopName='XX_Pampers', categoryName='*', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:31', deleteStatus=false, createTime='null', description='null'}
app_1           | 2026-04-18 02:24:42.038  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : processor data : Access{id=413, username='wu.j.5', shopName='XX_SKII', categoryName='*', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:31', deleteStatus=false, createTime='null', description='null'}
app_1           | 2026-04-18 02:24:42.038  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : processor data : Access{id=414, username='wu.j.5', shopName='XX_VS', categoryName='*', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:31', deleteStatus=false, createTime='null', description='null'}
app_1           | 2026-04-18 02:24:42.038  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : processor data : Access{id=415, username='wu.j.5', shopName='Tmall_Super', categoryName='*', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:31', deleteStatus=false, createTime='null', description='null'}
app_1           | 2026-04-18 02:24:42.038  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : processor data : Access{id=416, username='Zhou.k.5', shopName='JD', categoryName='*', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:32', deleteStatus=false, createTime='null', description='null'}
app_1           | 2026-04-18 02:24:42.038  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : processor data : Access{id=417, username='du.yu', shopName='*', categoryName='HAIR_CARE', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:32', deleteStatus=false, createTime='null', description='null'}
app_1           | 2026-04-18 02:24:42.038  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : processor data : Access{id=418, username='yuksel.s.1', shopName='*', categoryName='HAIR_CARE', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:32', deleteStatus=false, createTime='null', description='null'}
app_1           | 2026-04-18 02:24:42.038  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : processor data : Access{id=419, username='li.lu.1', shopName='*', categoryName='HAIR_CARE', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:32', deleteStatus=false, createTime='null', description='null'}
app_1           | 2026-04-18 02:24:42.038  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : processor data : Access{id=420, username='lee.km', shopName='*', categoryName='HAIR_CARE', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:32', deleteStatus=false, createTime='null', description='null'}
app_1           | 2026-04-18 02:24:42.038  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : processor data : Access{id=421, username='he.sw', shopName='VIP', categoryName='SKIN_CARE', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:32', deleteStatus=false, createTime='null', description='null'}
app_1           | 2026-04-18 02:24:42.038  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : processor data : Access{id=422, username='guan.we', shopName='VIP', categoryName='PRESTIGE', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:32', deleteStatus=false, createTime='null', description='null'}
app_1           | 2026-04-18 02:24:42.038  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : processor data : Access{id=423, username='zhang.z.26', shopName='VIP', categoryName='*', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:32', deleteStatus=false, createTime='null', description='null'}
app_1           | 2026-04-18 02:24:42.039  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : processor data : Access{id=424, username='zhang.z.26', shopName='SUNING', categoryName='*', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:32', deleteStatus=false, createTime='null', description='null'}
app_1           | 2026-04-18 02:24:42.039  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : processor data : Access{id=425, username='null', shopName='*', categoryName='*', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:32', deleteStatus=false, createTime='null', description='null'}
app_1           | 2026-04-18 02:24:42.039  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : processor data : Access{id=426, username='null', shopName='VIP', categoryName='PRESTIGE', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:32', deleteStatus=false, createTime='null', description='null'}
app_1           | 2026-04-18 02:24:42.039  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : processor data : Access{id=427, username='null', shopName='*', categoryName='*', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:32', deleteStatus=false, createTime='null', description='null'}
app_1           | 2026-04-18 02:24:42.039  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : write data : Access{id=401, username='lin.r.5', shopName='Tmall_Super', categoryName='*', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:31', deleteStatus=false, createTime='null', description='null'}
app_1           | 2026-04-18 02:24:42.039  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : write data : Access{id=402, username='zhang.yu.2', shopName='VIP', categoryName='*', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:31', deleteStatus=false, createTime='null', description='null'}
app_1           | 2026-04-18 02:24:42.039  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : write data : Access{id=403, username='zhang.yu.2', shopName='SUNING', categoryName='*', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:31', deleteStatus=false, createTime='null', description='null'}
app_1           | 2026-04-18 02:24:42.039  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : write data : Access{id=404, username='#', shopName='*', categoryName='HAIR_CARE', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:31', deleteStatus=false, createTime='null', description='null'}
app_1           | 2026-04-18 02:24:42.039  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : write data : Access{id=405, username='kang.j.4', shopName='*', categoryName='HAIR_CARE', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:31', deleteStatus=false, createTime='null', description='null'}
app_1           | 2026-04-18 02:24:42.039  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : write data : Access{id=406, username='wu.j.5', shopName='XX_Braun', categoryName='*', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:31', deleteStatus=false, createTime='null', description='null'}
app_1           | 2026-04-18 02:24:42.039  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : write data : Access{id=407, username='wu.j.5', shopName='XX_Gillette', categoryName='*', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:31', deleteStatus=false, createTime='null', description='null'}
app_1           | 2026-04-18 02:24:42.039  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : write data : Access{id=408, username='wu.j.5', shopName='XX_Global', categoryName='*', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:31', deleteStatus=false, createTime='null', description='null'}
app_1           | 2026-04-18 02:24:42.039  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : write data : Access{id=409, username='wu.j.5', shopName='XX_Olay', categoryName='*', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:31', deleteStatus=false, createTime='null', description='null'}
app_1           | 2026-04-18 02:24:42.040  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : write data : Access{id=410, username='wu.j.5', shopName='XX_Oral-B', categoryName='*', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:31', deleteStatus=false, createTime='null', description='null'}
app_1           | 2026-04-18 02:24:42.040  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : write data : Access{id=411, username='wu.j.5', shopName='XX_P&G', categoryName='*', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:31', deleteStatus=false, createTime='null', description='null'}
app_1           | 2026-04-18 02:24:42.040  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : write data : Access{id=412, username='wu.j.5', shopName='XX_Pampers', categoryName='*', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:31', deleteStatus=false, createTime='null', description='null'}
app_1           | 2026-04-18 02:24:42.040  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : write data : Access{id=413, username='wu.j.5', shopName='XX_SKII', categoryName='*', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:31', deleteStatus=false, createTime='null', description='null'}
app_1           | 2026-04-18 02:24:42.040  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : write data : Access{id=414, username='wu.j.5', shopName='XX_VS', categoryName='*', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:31', deleteStatus=false, createTime='null', description='null'}
app_1           | 2026-04-18 02:24:42.040  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : write data : Access{id=415, username='wu.j.5', shopName='Tmall_Super', categoryName='*', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:31', deleteStatus=false, createTime='null', description='null'}
app_1           | 2026-04-18 02:24:42.040  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : write data : Access{id=416, username='Zhou.k.5', shopName='JD', categoryName='*', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:32', deleteStatus=false, createTime='null', description='null'}
app_1           | 2026-04-18 02:24:42.040  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : write data : Access{id=417, username='du.yu', shopName='*', categoryName='HAIR_CARE', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:32', deleteStatus=false, createTime='null', description='null'}
app_1           | 2026-04-18 02:24:42.040  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : write data : Access{id=418, username='yuksel.s.1', shopName='*', categoryName='HAIR_CARE', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:32', deleteStatus=false, createTime='null', description='null'}
app_1           | 2026-04-18 02:24:42.040  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : write data : Access{id=419, username='li.lu.1', shopName='*', categoryName='HAIR_CARE', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:32', deleteStatus=false, createTime='null', description='null'}
app_1           | 2026-04-18 02:24:42.040  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : write data : Access{id=420, username='lee.km', shopName='*', categoryName='HAIR_CARE', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:32', deleteStatus=false, createTime='null', description='null'}
app_1           | 2026-04-18 02:24:42.040  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : write data : Access{id=421, username='he.sw', shopName='VIP', categoryName='SKIN_CARE', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:32', deleteStatus=false, createTime='null', description='null'}
app_1           | 2026-04-18 02:24:42.040  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : write data : Access{id=422, username='guan.we', shopName='VIP', categoryName='PRESTIGE', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:32', deleteStatus=false, createTime='null', description='null'}
app_1           | 2026-04-18 02:24:42.040  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : write data : Access{id=423, username='zhang.z.26', shopName='VIP', categoryName='*', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:32', deleteStatus=false, createTime='null', description='null'}
app_1           | 2026-04-18 02:24:42.040  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : write data : Access{id=424, username='zhang.z.26', shopName='SUNING', categoryName='*', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:32', deleteStatus=false, createTime='null', description='null'}
app_1           | 2026-04-18 02:24:42.040  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : write data : Access{id=425, username='null', shopName='*', categoryName='*', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:32', deleteStatus=false, createTime='null', description='null'}
app_1           | 2026-04-18 02:24:42.041  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : write data : Access{id=426, username='null', shopName='VIP', categoryName='PRESTIGE', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:32', deleteStatus=false, createTime='null', description='null'}
app_1           | 2026-04-18 02:24:42.042  INFO 1 --- [           main] name.ealen.batch.DataBatchConfiguration  : write data : Access{id=427, username='null', shopName='*', categoryName='*', brandName='null', shopId='null', omit='null', updateTime='2018-05-11 10:25:32', deleteStatus=false, createTime='null', description='null'}

app_1           | 2026-04-18 02:24:42.061  INFO 1 --- [           main] o.s.batch.core.step.AbstractStep         : Step: [getData] executed in 1s248ms
app_1           | 2026-04-18 02:24:42.075  INFO 1 --- [           main] name.ealen.listener.JobListener          : JOB STATUS : COMPLETED
app_1           | 2026-04-18 02:24:42.076  INFO 1 --- [           main] name.ealen.listener.JobListener          : JOB FINISHED
app_1           | 2026-04-18 02:24:42.077  INFO 1 --- [           main] o.s.s.concurrent.ThreadPoolTaskExecutor  : Shutting down ExecutorService 'threadPoolTaskExecutor'
app_1           | 2026-04-18 02:24:42.079  INFO 1 --- [           main] name.ealen.listener.JobListener          : Job Cost Time : 1317ms
app_1           | 2026-04-18 02:24:42.088  INFO 1 --- [           main] o.s.b.c.l.support.SimpleJobLauncher      : Job: [SimpleJob: [name=dataHandleJob]] completed with the following parameters: [{run.id=1}] and the following status: [COMPLETED] in 1s329ms
app_1           | 2026-04-18 02:24:42.100  INFO 1 --- [extShutdownHook] j.LocalContainerEntityManagerFactoryBean : Closing JPA EntityManagerFactory for persistence unit 'default'
app_1           | 2026-04-18 02:24:42.105  INFO 1 --- [extShutdownHook] o.s.s.concurrent.ThreadPoolTaskExecutor  : Shutting down ExecutorService 'threadPoolTaskExecutor'
app_1           | 2026-04-18 02:24:42.108  INFO 1 --- [extShutdownHook] com.zaxxer.hikari.HikariDataSource       : HikariPool-1 - Shutdown initiated...
app_1           | 2026-04-18 02:24:42.125  INFO 1 --- [extShutdownHook] com.zaxxer.hikari.HikariDataSource       : HikariPool-1 - Shutdown completed.

```

### Examination of Job Run
```
docker-compose exec mysql-server sh
```


```sh
mysql -u example_db_user -p
```

```sh
mysql> use example_db
```
```text

Database changed
```

```sh
mysql> select * from BATCH_JOB_INSTANCE;
```
```text
+-----------------+---------+---------------+----------------------------------+
| JOB_INSTANCE_ID | VERSION | JOB_NAME      | JOB_KEY                          |
+-----------------+---------+---------------+----------------------------------+
|               1 |       0 | dataHandleJob | 853d3449e311f40366811cbefb3d93d7 |
+-----------------+---------+---------------+----------------------------------+
```

```sh
mysql> select
    JOB_EXECUTION_ID,
    JOB_INSTANCE_ID,
    START_TIME,
    END_TIME,
    STATUS,
    EXIT_CODE,
    EXIT_MESSAGE,
    CREATE_TIME,
    LAST_UPDATED
from BATCH_JOB_EXECUTION;
```
```text
+------------------+-----------------+---------------------+---------------------+-----------+-----------+--------------+---------------------+---------------------+
| JOB_EXECUTION_ID | JOB_INSTANCE_ID | START_TIME          | END_TIME            | STATUS    | EXIT_CODE | EXIT_MESSAGE | CREATE_TIME         | LAST_UPDATED        |
+------------------+-----------------+---------------------+---------------------+-----------+-----------+--------------+---------------------+---------------------+
|                1 |               1 | 2026-04-18 02:24:41 | 2026-04-18 02:24:42 | COMPLETED | COMPLETED |              | 2026-04-18 02:24:41 | 2026-04-18 02:24:42 |
+------------------+-----------------+---------------------+---------------------+-----------+-----------+--------------+---------------------+---------------------+

```
```sh
mysql> select * from BATCH_JOB_EXECUTION_PARAMS;
```
```text
+------------------+---------+----------+------------+---------------------+----------+------------+-------------+
| JOB_EXECUTION_ID | TYPE_CD | KEY_NAME | STRING_VAL | DATE_VAL            | LONG_VAL | DOUBLE_VAL | IDENTIFYING |
+------------------+---------+----------+------------+---------------------+----------+------------+-------------+
|                1 | LONG    | run.id   |            | 1970-01-01 00:00:00 |        1 |          0 | Y           |
+------------------+---------+----------+------------+---------------------+----------+------------+-------------+

```

```sh
mysql> select
    STEP_EXECUTION_ID,
    STEP_NAME,
    START_TIME,
    END_TIME,
    STATUS,
    COMMIT_COUNT,
    READ_COUNT,
    WRITE_COUNT,
    FILTER_COUNT,
    READ_SKIP_COUNT,
    WRITE_SKIP_COUNT
from BATCH_STEP_EXECUTION;

```
```text
+-------------------+-----------+---------------------+---------------------+-----------+--------------+------------+-------------+--------------+-----------------+------------------+
| STEP_EXECUTION_ID | STEP_NAME | START_TIME          | END_TIME            | STATUS    | COMMIT_COUNT | READ_COUNT | WRITE_COUNT | FILTER_COUNT | READ_SKIP_COUNT | WRITE_SKIP_COUNT |
+-------------------+-----------+---------------------+---------------------+-----------+--------------+------------+-------------+--------------+-----------------+------------------+
|                 1 | getData   | 2026-04-18 02:24:41 | 2026-04-18 02:24:42 | COMPLETED |            5 |        427 |         427 |            0 |               0 |                0 |
+-------------------+-----------+---------------------+---------------------+-----------+--------------+------------+-------------+--------------+-----------------+------------------+
```

```sh
mysql> select
    ji.JOB_NAME,
    je.STATUS as JOB_STATUS,
    se.STEP_NAME,
    se.STATUS as STEP_STATUS,
    se.READ_COUNT,
    se.WRITE_COUNT
from BATCH_JOB_INSTANCE ji
join BATCH_JOB_EXECUTION je
    on ji.JOB_INSTANCE_ID = je.JOB_INSTANCE_ID
join BATCH_STEP_EXECUTION se
    on je.JOB_EXECUTION_ID = se.JOB_EXECUTION_ID;
```
```text
+---------------+------------+-----------+-------------+------------+-------------+
| JOB_NAME      | JOB_STATUS | STEP_NAME | STEP_STATUS | READ_COUNT | WRITE_COUNT |
+---------------+------------+-----------+-------------+------------+-------------+
| dataHandleJob | COMPLETED  | getData   | COMPLETED   |        427 |         427 |
+---------------+------------+-----------+-------------+------------+-------------+
```

### Re-Running

* if the `run.id` does not change one will get the `JobInstanceAlreadyCompleteException`

```sh
docker-compose ps
```
```         Name                         Command               State                   Ports
----------------------------------------------------------------------------------------------------------
basic-spring-batch2_app_1   sh -c until nc -z mysql-se ...   Exit 0
mysql-server                docker-entrypoint.sh mysqld      Up       0.0.0.0:3306->3306/tcp,:::3306-                                                                           >3306/tcp, 33060/tcp
```
```sh
docker-compose run app
```

repeating the SQL, see now
```text
+------------------+---------+----------+------------+---------------------+----------+------------+-------------+
| JOB_EXECUTION_ID | TYPE_CD | KEY_NAME | STRING_VAL | DATE_VAL            | LONG_VAL | DOUBLE_VAL | IDENTIFYING |
+------------------+---------+----------+------------+---------------------+----------+------------+-------------+
|                1 | LONG    | run.id   |            | 1970-01-01 00:00:00 |        1 |          0 | Y           |
|                2 | LONG    | run.id   |            | 1970-01-01 00:00:00 |        2 |          0 | Y           |
+------------------+---------+----------+------------+---------------------+----------+------------+-------------+
```

```sh
mysql> SELECT
         bse.STEP_NAME,
         bse.READ_COUNT,
         bse.WRITE_COUNT,
         bse.COMMIT_COUNT,
         bse.STATUS
     FROM BATCH_STEP_EXECUTION bse
     ORDER BY bse.STEP_EXECUTION_ID DESC;
```
```text
+-----------+------------+-------------+--------------+-----------+
| STEP_NAME | READ_COUNT | WRITE_COUNT | COMMIT_COUNT | STATUS    |
+-----------+------------+-------------+--------------+-----------+
| getData   |        427 |         427 |            5 | COMPLETED |
| getData   |        427 |         427 |            5 | COMPLETED |
+-----------+------------+-------------+--------------+-----------+
```

### Cleanup


```sh
docker-compose stop
docker-compose rm -f
```

```sh
rm -fr app/target
find . -type f | xargs -IX sed -i 's|\r$||g' X
```

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
