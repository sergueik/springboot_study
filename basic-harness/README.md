### Info

replica of [harness-cd-community](https://github.com/harness/harness-cd-community)

### Usage

```sh
pushd docker-compose/harnes
docker-compose up --build
```
NOTE: expect two digit load average on  a 16 GB 4 core laptop and expect networking changes while Harness cluster is running (inbound connections may be blocked)

```sh
ping 192.168.0.25
```
```text
PING 192.168.0.25 (192.168.0.25) 56(84) bytes of data.
From 192.168.0.1 icmp_seq=1 Destination Host Unreachable

```
presumably because the subnet `192.168.0.0/24` is used by `docker-compose.yml`:
```yml
networks:
  harness-network:
    ipam:
      config:
        - subnet: 192.168.0.0/24

```

```sh
docker network list
```
```text
NETWORK ID     NAME                             DRIVER    SCOPE
3b7541afd02d   basic-fullcycle-java17_default   bridge    local
1d6d95b8dccf   basic-perl-cgi_app               bridge    local
7af7a322f065   bridge                           bridge    local
1bb8675c1585   harness_harness-network          bridge    local
83ddadb0b1e0   host                             host      local
7369691904d9   none                             null      local
```
```sh
docker network prune -f
```
```text
Deleted Networks:
basic-fullcycle-java17_default
harness_harness-network
basic-perl-cgi_app
```

```sh
docker-compose ps
```
```text
docker-compose ps
NAME                         IMAGE                                       COMMAND                  SERVICE             CREATED             STATUS                      PORTS
harness-delegate-proxy-1     harness/delegate-proxy-signed:79310         "/bin/sh -c 'nginx -…"   delegate-proxy      47 minutes ago      Up 39 minutes (healthy)     8080/tcp
harness-log-service-1        harness/log-service-signed:release-70-ubi   "/usr/local/bin/log-…"   log-service         47 minutes ago      Up 39 minutes (healthy)     8079/tcp
harness-manager-1            harness/manager-signed:79421                "./run.sh"               manager             47 minutes ago      Up 39 minutes (healthy)     9090/tcp, 9879/tcp
harness-mongo-1              mongo:4.4.22                                "docker-entrypoint.s…"   mongo               47 minutes ago      Up 40 minutes (healthy)     27017/tcp
harness-ng-auth-ui-1         harness/ng-auth-ui-signed:1.7.0             "/bin/sh -c 'sed -i …"   ng-auth-ui          47 minutes ago      Up 39 minutes (healthy)     8080/tcp
harness-ng-manager-1         harness/ng-manager-signed:79421             "./run.sh"               ng-manager          47 minutes ago      Up 39 minutes (healthy)     7090/tcp
harness-ng-ui-1              harness/nextgenui-signed:0.353.10           "sh /opt/entrypoint.…"   ng-ui               47 minutes ago      Up 39 minutes (healthy)     8080/tcp
harness-pipeline-service-1   harness/pipeline-service-signed:1.33.8      "/opt/harness/run.sh"    pipeline-service    47 minutes ago      Up 40 minutes (healthy)     12001/tcp, 12011/tcp, 14002/tcp
harness-platform-service-1   harness/platform-service-signed:79202       "/opt/harness/run.sh"    platform-service    47 minutes ago      Exited (1) 38 minutes ago
harness-proxy-1              harness/nginx:1.21.4                        "/docker-entrypoint.…"   proxy               40 minutes ago      Up 39 minutes (healthy)     0.0.0.0:80->80/tcp, :::80->80/tcp, 0.0.0.0:9879->9879/tcp, :::9879->9879/tcp
harness-redis-1              harness/redis:6.2.7-alpine                  "docker-entrypoint.s…"   redis               47 minutes ago      Up 39 minutes (healthy)     6379/tcp
```

```sh
docker-compose logs platform-service
```
shows
```text
basic-harness-platform-service-1  | + [[ -v {hostname} ]]
basic-harness-platform-service-1  | + [[ -z 60m ]]
basic-harness-platform-service-1  | Using memory  60m
basic-harness-platform-service-1  | + [[ -z '' ]]
basic-harness-platform-service-1  | + export COMMAND=server
basic-harness-platform-service-1  | + COMMAND=server
basic-harness-platform-service-1  | + echo 'Using memory ' 60m
basic-harness-platform-service-1  | + [[ -z '' ]]
basic-harness-platform-service-1  | + export CAPSULE_JAR=/opt/harness/platform-service-capsule.jar
basic-harness-platform-service-1  | + CAPSULE_JAR=/opt/harness/platform-service-capsule.jar
basic-harness-platform-service-1  | + [[ true == \t\r\u\e ]]
basic-harness-platform-service-1  | + export 'GC_PARAMS= -XX:+UseSerialGC -Dfile.encoding=UTF-8'
basic-harness-platform-service-1  | + GC_PARAMS=' -XX:+UseSerialGC -Dfile.encoding=UTF-8'
basic-harness-platform-service-1  | + export 'JAVA_OPTS=-Xmx60m -XX:+HeapDumpOnOutOfMemoryError -Xloggc:mygclogfilename.gc  -XX:+UseSerialGC -Dfile.encoding=UTF-8 -XX:-TieredCompilation'
basic-harness-platform-service-1  | + JAVA_OPTS='-Xmx60m -XX:+HeapDumpOnOutOfMemoryError -Xloggc:mygclogfilename.gc  -XX:+UseSerialGC -Dfile.encoding=UTF-8 -XX:-TieredCompilation'
basic-harness-platform-service-1  | + [[ '' == \t\r\u\e ]]
basic-harness-platform-service-1  | + [[ '' == \t\r\u\e ]]
basic-harness-platform-service-1  | + [[ KUBERNETES_ONPREM == \K\U\B\E\R\N\E\T\E\S ]]
basic-harness-platform-service-1  | + [[ KUBERNETES_ONPREM == \K\U\B\E\R\N\E\T\E\S\_\O\N\P\R\E\M ]]
basic-harness-platform-service-1  | + java -Xmx60m -XX:+HeapDumpOnOutOfMemoryError -Xloggc:mygclogfilename.gc -XX:+UseSerialGC -Dfile.encoding=UTF-8 -XX:-TieredCompilation -jar /opt/harness/platform-service-capsule.jar server /opt/harness/config.yml
basic-harness-platform-service-1  | [0.002s][warning][gc] -Xloggc is deprecated. Will use -Xlog:gc:mygclogfilename.gc instead.
basic-harness-platform-service-1  | ERROR StatusLogger Log4j2 could not find a logging implementation. Please add log4j-core to the classpath. Using SimpleLogger to log to the console...
basic-harness-platform-service-1  | WARN  [2024-02-21 23:06:11,131] org.reflections.Reflections: given scan urls are empty. set urls in the configuration
basic-harness-platform-service-1  | WARN  [2024-02-21 23:06:14,149] javax.persistence.spi: javax.persistence.spi::No valid providers found.
basic-harness-platform-service-1  | WARNING: An illegal reflective access operation has occurred
basic-harness-platform-service-1  | WARNING: Illegal reflective access by org.springframework.util.ReflectionUtils (file:/opt/harness/platform-service-capsule.jar) to field java.lang.Enum.name
basic-harness-platform-service-1  | WARNING: Please consider reporting this to the maintainers of org.springframework.util.ReflectionUtils
basic-harness-platform-service-1  | WARNING: Use --illegal-access=warn to enable warnings of further illegal reflective access operations
basic-harness-platform-service-1  | WARNING: All illegal access operations will be denied in a future release
basic-harness-platform-service-1  | 2024-02-21 23:06:52,987 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.mongo.IndexManagerSession[0;39m - No class store collection filters with not final class io.harness.filter.entity.Filter [33m[collectionName=filters][0;39m
basic-harness-platform-service-1  | 2024-02-21 23:06:53,512 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.mongo.IndexManagerSession[0;39m - Creating index {"name": "unique_accountId_organizationId_projectId_type", "unique": true} {"fullyQualifiedIdentifier": 1, "filterType": 1} [33m[indexName=unique_accountId_organizationId_projectId_type, collectionName=filters][0;39m
basic-harness-platform-service-1  | 2024-02-21 23:06:54,349 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.mongo.IndexManagerSession[0;39m - Creating index {"name": "unique_accountId_orgId_projectId_name_type_Index", "unique": true} {"accountIdentifier": 1, "orgIdentifier": 1, "projectIdentifier": 1, "name": 1, "filterType": 1} [33m[indexName=unique_accountId_orgId_projectId_name_type_Index, collectionName=filters][0;39m
basic-harness-platform-service-1  | 2024-02-21 23:06:54,534 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.mongo.IndexManagerSession[0;39m - No class store collection notifyQueue with not final class io.harness.waiter.NotifyEvent [33m[collectionName=notifyQueue][0;39m
basic-harness-platform-service-1  | 2024-02-21 23:06:54,539 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.mongo.IndexManagerSession[0;39m - Creating index {"name": "waitInstanceId_1", "unique": true} {"waitInstanceId": 1} [33m[indexName=waitInstanceId_1, collectionName=notifyQueue][0;39m
basic-harness-platform-service-1  | 2024-02-21 23:06:54,972 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.mongo.IndexManagerSession[0;39m - Creating index {"name": "earliestGet_1", "background": true, "expireAfterSeconds": 86400} {"earliestGet": 1} [33m[indexName=earliestGet_1, collectionName=notifyQueue][0;39m
basic-harness-platform-service-1  | 2024-02-21 23:06:55,018 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.mongo.IndexManagerSession[0;39m - Creating index {"name": "next4", "background": true} {"topic": 1, "earliestGet": 1} [33m[indexName=next4, collectionName=notifyQueue][0;39m
basic-harness-platform-service-1  | 2024-02-21 23:06:55,332 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.mongo.IndexManagerSession[0;39m - Creating index {"name": "processAfter_1", "background": true} {"processAfter": 1} [33m[indexName=processAfter_1, collectionName=ns_delegateAsyncTaskResponses][0;39m
basic-harness-platform-service-1  | 2024-02-21 23:06:55,434 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.mongo.IndexManagerSession[0;39m - Creating index {"name": "validUntil_1", "background": true, "expireAfterSeconds": 0} {"validUntil": 1} [33m[indexName=validUntil_1, collectionName=ns_delegateAsyncTaskResponses][0;39m
basic-harness-platform-service-1  | 2024-02-21 23:06:55,496 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.mongo.IndexManagerSession[0;39m - Class store collection cache with final class io.harness.cache.CacheEntity [33m[collectionName=cache][0;39m
basic-harness-platform-service-1  | 2024-02-21 23:06:55,510 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.mongo.IndexManagerSession[0;39m - Creating index {"name": "accountId_1", "background": true} {"accountId": 1} [33m[indexName=accountId_1, collectionName=cache][0;39m
basic-harness-platform-service-1  | 2024-02-21 23:06:55,621 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.mongo.IndexManagerSession[0;39m - Creating index {"name": "validUntil_1", "background": true, "expireAfterSeconds": 0} {"validUntil": 1} [33m[indexName=validUntil_1, collectionName=cache][0;39m
basic-harness-platform-service-1  | 2024-02-21 23:06:55,663 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.mongo.IndexManagerSession[0;39m - Creating index {"name": "unique_commutative", "unique": true} {"_id": 1, "contextValue": 1} [33m[indexName=unique_commutative, collectionName=cache][0;39m
basic-harness-platform-service-1  | 2024-02-21 23:06:55,803 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.mongo.IndexManagerSession[0;39m - Creating index {"name": "unique_notificationsetting_idx", "unique": true} {"accountId": 1} [33m[indexName=unique_notificationsetting_idx, collectionName=notificationSettings][0;39m
basic-harness-platform-service-1  | 2024-02-21 23:06:56,069 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.mongo.IndexManagerSession[0;39m - Creating index {"name": "team_idx", "background": true} {"team": 1} [33m[indexName=team_idx, collectionName=notificationTemplates][0;39m
basic-harness-platform-service-1  | 2024-02-21 23:06:56,466 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.mongo.IndexManagerSession[0;39m - Creating index {"name": "unique_identifier_team_idx", "unique": true} {"identifier": 1, "team": 1} [33m[indexName=unique_identifier_team_idx, collectionName=notificationTemplates][0;39m
basic-harness-platform-service-1  | 2024-02-21 23:06:56,611 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.mongo.IndexManagerSession[0;39m - No class store collection notificationsNg with not final class io.harness.notification.entities.Notification [33m[collectionName=notificationsNg][0;39m
basic-harness-platform-service-1  | 2024-02-21 23:06:56,621 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.mongo.IndexManagerSession[0;39m - Creating index {"name": "unique_notification_idx", "unique": true} {"id": 1} [33m[indexName=unique_notification_idx, collectionName=notificationsNg][0;39m
basic-harness-platform-service-1  | 2024-02-21 23:06:56,809 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.mongo.IndexManagerSession[0;39m - Creating index {"name": "notification_retries_sent", "background": true} {"shouldRetry": 1, "retries": 1} [33m[indexName=notification_retries_sent, collectionName=notificationsNg][0;39m
basic-harness-platform-service-1  | 2024-02-21 23:06:56,986 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.mongo.IndexManagerSession[0;39m - Creating index {"name": "nextIteration_1", "background": true} {"nextIteration": 1} [33m[indexName=nextIteration_1, collectionName=notificationsNg][0;39m
basic-harness-platform-service-1  | 2024-02-21 23:06:57,142 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.mongo.IndexManagerSession[0;39m - No class store collection migrationJobInstances with not final class io.harness.migration.MigrationJobInstance [33m[collectionName=migrationJobInstances][0;39m
basic-harness-platform-service-1  | 2024-02-21 23:06:57,167 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.mongo.IndexManagerSession[0;39m - Creating index {"name": "validUntil_1", "background": true, "expireAfterSeconds": 0} {"validUntil": 1} [33m[indexName=validUntil_1, collectionName=progressUpdate][0;39m
basic-harness-platform-service-1  | 2024-02-21 23:06:57,433 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.mongo.IndexManagerSession[0;39m - Creating index {"name": "correlation", "background": true} {"correlationId": 1, "createdAt": -1} [33m[indexName=correlation, collectionName=progressUpdate][0;39m
basic-harness-platform-service-1  | 2024-02-21 23:06:57,507 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.mongo.IndexManagerSession[0;39m - Creating index {"name": "createdAt_1", "background": true} {"createdAt": 1} [33m[indexName=createdAt_1, collectionName=progressUpdate][0;39m
basic-harness-platform-service-1  | 2024-02-21 23:06:57,585 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.mongo.IndexManagerSession[0;39m - Creating index {"name": "validUntil_1", "background": true, "expireAfterSeconds": 0} {"validUntil": 1} [33m[indexName=validUntil_1, collectionName=ns_delegateSyncTaskResponses][0;39m
basic-harness-platform-service-1  | 2024-02-21 23:06:57,776 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.mongo.IndexManagerSession[0;39m - Creating index {"name": "validUntil_1", "background": true, "expireAfterSeconds": 0} {"validUntil": 1} [33m[indexName=validUntil_1, collectionName=notifyResponses][0;39m
basic-harness-platform-service-1  | 2024-02-21 23:06:57,867 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.mongo.IndexManagerSession[0;39m - Creating index {"name": "createdAt_1", "background": true} {"createdAt": 1} [33m[indexName=createdAt_1, collectionName=notifyResponses][0;39m
basic-harness-platform-service-1  | 2024-02-21 23:06:57,933 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.mongo.IndexManagerSession[0;39m - Creating index {"name": "correlationIds_1", "background": true} {"correlationIds": 1} [33m[indexName=correlationIds_1, collectionName=waitInstances][0;39m
basic-harness-platform-service-1  | 2024-02-21 23:06:58,043 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.mongo.IndexManagerSession[0;39m - Creating index {"name": "validUntil_1", "background": true, "expireAfterSeconds": 0} {"validUntil": 1} [33m[indexName=validUntil_1, collectionName=waitInstances][0;39m
basic-harness-platform-service-1  | 2024-02-21 23:06:58,191 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.mongo.IndexManagerSession[0;39m - Creating index {"name": "waitingOnCorrelationIds_1", "background": true} {"waitingOnCorrelationIds": 1} [33m[indexName=waitingOnCorrelationIds_1, collectionName=waitInstances][0;39m
basic-harness-platform-service-1  | 2024-02-21 23:06:58,287 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.mongo.IndexManagerSession[0;39m - Creating index {"name": "processAfter_1", "background": true} {"processAfter": 1} [33m[indexName=processAfter_1, collectionName=ns_delegateTaskProgressResponses][0;39m
basic-harness-platform-service-1  | 2024-02-21 23:06:58,391 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.mongo.IndexManagerSession[0;39m - Creating index {"name": "validUntil_1", "background": true, "expireAfterSeconds": 0} {"validUntil": 1} [33m[indexName=validUntil_1, collectionName=ns_delegateTaskProgressResponses][0;39m
basic-harness-platform-service-1  | 2024-02-21 23:07:03,440 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.notification.service.SeedDataPopulaterServiceImpl[0;39m - Resource not found, skipping to seed the template - java.lang.IllegalArgumentException: resource notification_templates/pipeline/slack/plain_text.txt not found. [33m[0;39m
basic-harness-platform-service-1  | 2024-02-21 23:07:03,444 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.notification.service.SeedDataPopulaterServiceImpl[0;39m - Resource not found, skipping to seed the template - java.lang.IllegalArgumentException: resource notification_templates/pipeline/email/plain_text.txt not found. [33m[0;39m
basic-harness-platform-service-1  | 2024-02-21 23:07:03,450 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.notification.service.SeedDataPopulaterServiceImpl[0;39m - Resource not found, skipping to seed the template - java.lang.IllegalArgumentException: resource notification_templates/pipeline/pagerduty/plain_text.txt not found. [33m[0;39m
basic-harness-platform-service-1  | 2024-02-21 23:07:03,453 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.notification.service.SeedDataPopulaterServiceImpl[0;39m - Resource not found, skipping to seed the template - java.lang.IllegalArgumentException: resource notification_templates/pipeline/msteams/plain_text.txt not found. [33m[0;39m
basic-harness-platform-service-1  | 2024-02-21 23:07:03,459 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.notification.service.SeedDataPopulaterServiceImpl[0;39m - Resource not found, skipping to seed the template - java.lang.IllegalArgumentException: resource notification_templates/stage/slack/plain_text.txt not found. [33m[0;39m
basic-harness-platform-service-1  | 2024-02-21 23:07:03,464 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.notification.service.SeedDataPopulaterServiceImpl[0;39m - Resource not found, skipping to seed the template - java.lang.IllegalArgumentException: resource notification_templates/stage/email/plain_text.txt not found. [33m[0;39m
basic-harness-platform-service-1  | 2024-02-21 23:07:03,469 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.notification.service.SeedDataPopulaterServiceImpl[0;39m - Resource not found, skipping to seed the template - java.lang.IllegalArgumentException: resource notification_templates/stage/pagerduty/plain_text.txt not found. [33m[0;39m
basic-harness-platform-service-1  | 2024-02-21 23:07:03,478 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.notification.service.SeedDataPopulaterServiceImpl[0;39m - Resource not found, skipping to seed the template - java.lang.IllegalArgumentException: resource notification_templates/stage/msteams/plain_text.txt not found. [33m[0;39m
basic-harness-platform-service-1  | 2024-02-21 23:07:03,485 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.notification.service.SeedDataPopulaterServiceImpl[0;39m - Resource not found, skipping to seed the template - java.lang.IllegalArgumentException: resource notification_templates/step/slack/plain_text.txt not found. [33m[0;39m
basic-harness-platform-service-1  | 2024-02-21 23:07:03,491 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.notification.service.SeedDataPopulaterServiceImpl[0;39m - Resource not found, skipping to seed the template - java.lang.IllegalArgumentException: resource notification_templates/step/email/plain_text.txt not found. [33m[0;39m
basic-harness-platform-service-1  | 2024-02-21 23:07:03,495 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.notification.service.SeedDataPopulaterServiceImpl[0;39m - Resource not found, skipping to seed the template - java.lang.IllegalArgumentException: resource notification_templates/step/pagerduty/plain_text.txt not found. [33m[0;39m
basic-harness-platform-service-1  | 2024-02-21 23:07:03,499 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.notification.service.SeedDataPopulaterServiceImpl[0;39m - Resource not found, skipping to seed the template - java.lang.IllegalArgumentException: resource notification_templates/step/msteams/plain_text.txt not found. [33m[0;39m
basic-harness-platform-service-1  | 2024-02-21 23:07:03,503 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.notification.service.SeedDataPopulaterServiceImpl[0;39m - Resource not found, skipping to seed the template - java.lang.IllegalArgumentException: resource notification_templates/approval/slack/plain_text.txt not found. [33m[0;39m
basic-harness-platform-service-1  | 2024-02-21 23:07:03,505 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.notification.service.SeedDataPopulaterServiceImpl[0;39m - Resource not found, skipping to seed the template - java.lang.IllegalArgumentException: resource notification_templates/approval/email/plain_text.txt not found. [33m[0;39m
basic-harness-platform-service-1  | 2024-02-21 23:07:03,508 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.notification.service.SeedDataPopulaterServiceImpl[0;39m - Resource not found, skipping to seed the template - java.lang.IllegalArgumentException: resource notification_templates/approval/slack/plain_text_execution.txt not found. [33m[0;39m
basic-harness-platform-service-1  | 2024-02-21 23:07:03,512 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.notification.service.SeedDataPopulaterServiceImpl[0;39m - Resource not found, skipping to seed the template - java.lang.IllegalArgumentException: resource notification_templates/approval/email/plain_text_execution.txt not found. [33m[0;39m
basic-harness-platform-service-1  | 2024-02-21 23:07:03,517 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.notification.service.SeedDataPopulaterServiceImpl[0;39m - Resource not found, skipping to seed the template - java.lang.IllegalArgumentException: resource cvng_notification_templates/cvng_slo_simple_project_slack.txt not found. [33m[0;39m
basic-harness-platform-service-1  | 2024-02-21 23:07:03,520 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.notification.service.SeedDataPopulaterServiceImpl[0;39m - Resource not found, skipping to seed the template - java.lang.IllegalArgumentException: resource cvng_notification_templates/cvng_slo_simple_project_email.txt not found. [33m[0;39m
basic-harness-platform-service-1  | 2024-02-21 23:07:03,523 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.notification.service.SeedDataPopulaterServiceImpl[0;39m - Resource not found, skipping to seed the template - java.lang.IllegalArgumentException: resource cvng_notification_templates/cvng_slo_simple_project_pagerduty.txt not found. [33m[0;39m
basic-harness-platform-service-1  | 2024-02-21 23:07:03,526 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.notification.service.SeedDataPopulaterServiceImpl[0;39m - Resource not found, skipping to seed the template - java.lang.IllegalArgumentException: resource cvng_notification_templates/cvng_slo_simple_project_msteams.txt not found. [33m[0;39m
basic-harness-platform-service-1  | 2024-02-21 23:07:03,528 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.notification.service.SeedDataPopulaterServiceImpl[0;39m - Resource not found, skipping to seed the template - java.lang.IllegalArgumentException: resource cvng_notification_templates/cvng_slo_composite_project_slack.txt not found. [33m[0;39m
basic-harness-platform-service-1  | 2024-02-21 23:07:03,531 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.notification.service.SeedDataPopulaterServiceImpl[0;39m - Resource not found, skipping to seed the template - java.lang.IllegalArgumentException: resource cvng_notification_templates/cvng_slo_composite_project_email.txt not found. [33m[0;39m
basic-harness-platform-service-1  | 2024-02-21 23:07:03,533 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.notification.service.SeedDataPopulaterServiceImpl[0;39m - Resource not found, skipping to seed the template - java.lang.IllegalArgumentException: resource cvng_notification_templates/cvng_slo_composite_project_pagerduty.txt not found. [33m[0;39m
basic-harness-platform-service-1  | 2024-02-21 23:07:03,542 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.notification.service.SeedDataPopulaterServiceImpl[0;39m - Resource not found, skipping to seed the template - java.lang.IllegalArgumentException: resource cvng_notification_templates/cvng_slo_composite_project_msteams.txt not found. [33m[0;39m
basic-harness-platform-service-1  | 2024-02-21 23:07:03,547 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.notification.service.SeedDataPopulaterServiceImpl[0;39m - Resource not found, skipping to seed the template - java.lang.IllegalArgumentException: resource cvng_notification_templates/cvng_slo_composite_account_slack.txt not found. [33m[0;39m
basic-harness-platform-service-1  | 2024-02-21 23:07:03,551 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.notification.service.SeedDataPopulaterServiceImpl[0;39m - Resource not found, skipping to seed the template - java.lang.IllegalArgumentException: resource cvng_notification_templates/cvng_slo_composite_account_email.txt not found. [33m[0;39m
basic-harness-platform-service-1  | 2024-02-21 23:07:03,554 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.notification.service.SeedDataPopulaterServiceImpl[0;39m - Resource not found, skipping to seed the template - java.lang.IllegalArgumentException: resource cvng_notification_templates/cvng_slo_composite_account_pagerduty.txt not found. [33m[0;39m
basic-harness-platform-service-1  | 2024-02-21 23:07:03,558 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.notification.service.SeedDataPopulaterServiceImpl[0;39m - Resource not found, skipping to seed the template - java.lang.IllegalArgumentException: resource cvng_notification_templates/cvng_slo_composite_account_msteams.txt not found. [33m[0;39m
basic-harness-platform-service-1  | 2024-02-21 23:07:03,561 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.notification.service.SeedDataPopulaterServiceImpl[0;39m - Resource not found, skipping to seed the template - java.lang.IllegalArgumentException: resource cvng_notification_templates/cvng_monitoredservice_slack.txt not found. [33m[0;39m
basic-harness-platform-service-1  | 2024-02-21 23:07:03,564 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.notification.service.SeedDataPopulaterServiceImpl[0;39m - Resource not found, skipping to seed the template - java.lang.IllegalArgumentException: resource cvng_notification_templates/cvng_monitoredservice_et_slack.txt not found. [33m[0;39m
basic-harness-platform-service-1  | 2024-02-21 23:07:03,567 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.notification.service.SeedDataPopulaterServiceImpl[0;39m - Resource not found, skipping to seed the template - java.lang.IllegalArgumentException: resource cvng_notification_templates/cvng_monitoredservice_email.txt not found. [33m[0;39m
basic-harness-platform-service-1  | 2024-02-21 23:07:03,572 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.notification.service.SeedDataPopulaterServiceImpl[0;39m - Resource not found, skipping to seed the template - java.lang.IllegalArgumentException: resource cvng_notification_templates/cvng_monitoredservice_et_email.txt not found. [33m[0;39m
basic-harness-platform-service-1  | 2024-02-21 23:07:03,574 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.notification.service.SeedDataPopulaterServiceImpl[0;39m - Resource not found, skipping to seed the template - java.lang.IllegalArgumentException: resource cvng_notification_templates/cvng_monitoredservice_pagerduty.txt not found. [33m[0;39m
basic-harness-platform-service-1  | 2024-02-21 23:07:03,578 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.notification.service.SeedDataPopulaterServiceImpl[0;39m - Resource not found, skipping to seed the template - java.lang.IllegalArgumentException: resource cvng_notification_templates/cvng_monitoredservice_msteams.txt not found. [33m[0;39m
basic-harness-platform-service-1  | 2024-02-21 23:07:03,632 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.notification.service.SeedDataPopulaterServiceImpl[0;39m - Resource not found, skipping to seed the template - java.lang.IllegalArgumentException: resource notification_templates/email_test.txt not found. [33m[0;39m
basic-harness-platform-service-1  | 2024-02-21 23:07:03,633 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.notification.service.SeedDataPopulaterServiceImpl[0;39m - Resource not found, skipping to seed the template - java.lang.IllegalArgumentException: resource notification_templates/slack_test.txt not found. [33m[0;39m
basic-harness-platform-service-1  | 2024-02-21 23:07:03,636 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.notification.service.SeedDataPopulaterServiceImpl[0;39m - Resource not found, skipping to seed the template - java.lang.IllegalArgumentException: resource notification_templates/pd_test.txt not found. [33m[0;39m
basic-harness-platform-service-1  | 2024-02-21 23:07:03,639 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.notification.service.SeedDataPopulaterServiceImpl[0;39m - Resource not found, skipping to seed the template - java.lang.IllegalArgumentException: resource notification_templates/msteams_test.txt not found. [33m[0;39m
basic-harness-platform-service-1  | 2024-02-21 23:07:03,652 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.notification.service.SeedDataPopulaterServiceImpl[0;39m - Resource not found, skipping to seed the template - java.lang.IllegalArgumentException: resource notification_templates/pipeline_rejected_email_test.txt not found. [33m[0;39m
basic-harness-platform-service-1  | 2024-02-21 23:07:03,654 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.notification.service.SeedDataPopulaterServiceImpl[0;39m - Resource not found, skipping to seed the template - java.lang.IllegalArgumentException: resource notification_templates/pipeline_rejected_slack_test.txt not found. [33m[0;39m
basic-harness-platform-service-1  | 2024-02-21 23:07:03,656 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.notification.service.SeedDataPopulaterServiceImpl[0;39m - Resource not found, skipping to seed the template - java.lang.IllegalArgumentException: resource notification_templates/pipeline_rejected_pd_test.txt not found. [33m[0;39m
basic-harness-platform-service-1  | 2024-02-21 23:07:03,659 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.notification.service.SeedDataPopulaterServiceImpl[0;39m - Resource not found, skipping to seed the template - java.lang.IllegalArgumentException: resource notification_templates/pipeline_rejected_msteams_test.txt not found. [33m[0;39m
basic-harness-platform-service-1  | 2024-02-21 23:07:03,661 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.notification.service.SeedDataPopulaterServiceImpl[0;39m - Resource not found, skipping to seed the template - java.lang.IllegalArgumentException: resource notification_templates/approval/msteams/plain_text.txt not found. [33m[0;39m
basic-harness-platform-service-1  | 2024-02-21 23:07:03,670 [32m[main][0;39m [31mWARN [0;39m [36mio.harness.notification.service.SeedDataPopulaterServiceImpl[0;39m - Resource not found, skipping to seed the template - java.lang.IllegalArgumentException: resource notification_templates/approval/msteams/plain_text_execution.txt not found. [33m[0;39m
basic-harness-platform-service-1  | java.lang.OutOfMemoryError: Java heap space
basic-harness-platform-service-1  | Dumping heap to java_pid149.hprof ...
basic-harness-platform-service-1  | Heap dump file created [93389809 bytes in 9.588 secs]
basic-harness-platform-service-1  | java.lang.OutOfMemoryError: Java heap space
basic-harness-platform-service-1  | 	at java.base/java.util.Arrays.copyOf(Arrays.java:3745)
basic-harness-platform-service-1  | 	at java.base/jdk.internal.loader.Resource.getBytes(Resource.java:117)
basic-harness-platform-service-1  | 	at java.base/jdk.internal.loader.URLClassPath$JarLoader$2.getBytes(URLClassPath.java:882)
basic-harness-platform-service-1  | 	at java.base/jdk.internal.loader.BuiltinClassLoader.defineClass(BuiltinClassLoader.java:797)
basic-harness-platform-service-1  | 	at java.base/jdk.internal.loader.BuiltinClassLoader.findClassOnClassPathOrNull(BuiltinClassLoader.java:698)
basic-harness-platform-service-1  | 	at java.base/jdk.internal.loader.BuiltinClassLoader.loadClassOrNull(BuiltinClassLoader.java:621)
basic-harness-platform-service-1  | 	at java.base/jdk.internal.loader.BuiltinClassLoader.loadClass(BuiltinClassLoader.java:579)
basic-harness-platform-service-1  | 	at java.base/jdk.internal.loader.ClassLoaders$AppClassLoader.loadClass(ClassLoaders.java:178)
basic-harness-platform-service-1  | 	at java.base/java.lang.ClassLoader.loadClass(ClassLoader.java:522)
basic-harness-platform-service-1  | 	at java.base/java.lang.Class.getDeclaredMethods0(Native Method)
basic-harness-platform-service-1  | 	at java.base/java.lang.Class.privateGetDeclaredMethods(Class.java:3166)
basic-harness-platform-service-1  | 	at java.base/java.lang.Class.getDeclaredMethods(Class.java:2309)
basic-harness-platform-service-1  | 	at org.glassfish.jersey.server.model.IntrospectionModeller$2.run(IntrospectionModeller.java:236)
basic-harness-platform-service-1  | 	at java.base/java.security.AccessController.doPrivileged(Native Method)
basic-harness-platform-service-1  | 	at org.glassfish.jersey.server.model.IntrospectionModeller.getAllDeclaredMethods(IntrospectionModeller.java:230)
basic-harness-platform-service-1  | 	at org.glassfish.jersey.server.model.IntrospectionModeller.checkForNonPublicMethodIssues(IntrospectionModeller.java:150)
basic-harness-platform-service-1  | 	at org.glassfish.jersey.server.model.IntrospectionModeller.doCreateResourceBuilder(IntrospectionModeller.java:97)
basic-harness-platform-service-1  | 	at org.glassfish.jersey.server.model.IntrospectionModeller.access$000(IntrospectionModeller.java:58)
basic-harness-platform-service-1  | 	at org.glassfish.jersey.server.model.IntrospectionModeller$1.call(IntrospectionModeller.java:90)
basic-harness-platform-service-1  | 	at org.glassfish.jersey.server.model.IntrospectionModeller$1.call(IntrospectionModeller.java:87)
basic-harness-platform-service-1  | 	at org.glassfish.jersey.internal.Errors.process(Errors.java:292)
basic-harness-platform-service-1  | 	at org.glassfish.jersey.internal.Errors.process(Errors.java:274)
basic-harness-platform-service-1  | 	at org.glassfish.jersey.internal.Errors.processWithException(Errors.java:232)
basic-harness-platform-service-1  | 	at org.glassfish.jersey.server.model.IntrospectionModeller.createResourceBuilder(IntrospectionModeller.java:87)
basic-harness-platform-service-1  | 	at org.glassfish.jersey.server.model.Resource.from(Resource.java:781)
basic-harness-platform-service-1  | 	at org.glassfish.jersey.server.ResourceBagConfigurator.init(ResourceBagConfigurator.java:66)
basic-harness-platform-service-1  | 	at org.glassfish.jersey.server.ApplicationHandler.initialize(ApplicationHandler.java:331)
basic-harness-platform-service-1  | 	at org.glassfish.jersey.server.ApplicationHandler.lambda$initialize$1(ApplicationHandler.java:293)
basic-harness-platform-service-1  | 	at org.glassfish.jersey.server.ApplicationHandler$$Lambda$1044/0x0000000100a89840.call(Unknown Source)
basic-harness-platform-service-1  | 	at org.glassfish.jersey.internal.Errors.process(Errors.java:292)
basic-harness-platform-service-1  | 	at org.glassfish.jersey.internal.Errors.process(Errors.java:274)
basic-harness-platform-service-1  | 	at org.glassfish.jersey.internal.Errors.processWithException(Errors.java:232)

```
is fixe through updating the onfiguration
```yaml
 platform-service:
    image: harness/platform-service-signed:79202
    deploy:
      resources:
        limits:
          memory: 244m
```
to
```yaml
 platform-service:
    image: harness/platform-service-signed:79202
    deploy:
      resources:
        limits:
          memory: 1024m

```
and in `environment/platform-service.env`:
```text
MEMORY=60m
```
to
```text
MEMORY=1024m

```
and rerunning
```sh
docker-compose up --build platform-service
```
![Harness Signup](https://github.com/sergueik/springboot_study/blob/master/basic-harness/screenshots/capture-harness-signup.png)

NOTE: the password will not authenticate you anywhere but this cluster

![Harness Login](https://github.com/sergueik/springboot_study/blob/master/basic-harness/screenshots/capture-harness-login.png)

### Cleanup
```sh
docker-compose stop
docker network prune -f
```

```text
Deleted Networks:
basic-harness_harness-network
```
### WinRM

#### Windows host

* enable WinRM with basic authentication over HTTP

```powershell
enable-psremoting -force
winrm set winrm/config/service '@{AllowUnencrypted="true"}'
winrm set  winrm/config/service/auth '@{Basic="true"}
```
this establishes the channel without encryption and with basic authentication

* execute some Powershell to have identifyable result
```powershell
get-wmiobject "win32_computersystem"
```

```text
Domain              : WORKGROUP
Manufacturer        : innotek GmbH
Model               : VirtualBox
Name                : WIN-NCC616RSI3V
PrimaryOwnerName    :
TotalPhysicalMemory : 2147012608
```

![Windows Target](https://github.com/sergueik/springboot_study/blob/master/basic-harness/screenshots/capture-windows-server.png)

#### Linux Host

```sh
sudo pip3 install pywinrm
sudo pip3 install winrmcp
```
```sh
python
```

in Python shell

```
>>> import winrm
>>> session = winrm.Session('192.168.0.185', auth = ('administrator', 'NP730qfg'))
>>> result = session.run_ps('get-wmiobject "win32_computersystem"')
>>> print(result.std_out.decode('utf8'))
```

```text
Domain              : WORKGROUP
Manufacturer        : innotek GmbH
Model               : VirtualBox
Name                : WIN-NCC616RSI3V
PrimaryOwnerName    :
TotalPhysicalMemory : 2147012608

```




```python
>>> result = session.run_ps('get-content "c:\\users\\administrator\\Desktop\\a.txt"')
>>> print(result.std_out.decode('utf8'))
```


```text
hello World
```

* in Python shell
```sh
python3
```
```python
>>> from winrmcp import Client
>>> client = Client('192.168.0.185', auth = ('administrator', 'NP730qfg'))
>>> client.copy('/home/sergueik/test.ps1', '%TEMP%\\test.ps1')
>>> with client.shell() as shell:
    shell.check_call('powershell.exe', '-executionpolicy', 'bypass', '-noprofile', '-file', '%TEMP%\\test.ps1')
```

this does not work presumably because `winrmcp` is not Python3 compatible (same error thrown from `client.copy` and `shell.check_call`:

```text
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
  File "/usr/local/lib/python3.10/dist-packages/winrmcp/client.py", line 26, in copy
    do_copy(self, f, to_file, max_operations_per_shell=15)
  File "/usr/local/lib/python3.10/dist-packages/winrmcp/client.py", line 202, in do_copy
    result = upload_chunks(shell, f'%TEMP%\\{temp_file}', max_operations_per_shell, fileobj)
  File "/usr/local/lib/python3.10/dist-packages/winrmcp/client.py", line 190, in upload_chunks
    shell.check_cmd(['echo', chunk, '>>', file_path])
  File "/usr/local/lib/python3.10/dist-packages/winrmcp/client.py", line 41, in check_cmd
    return self._check(self.cmd(cmd, *args))
  File "/usr/local/lib/python3.10/dist-packages/winrmcp/client.py", line 35, in cmd
    command_id = self.protocol.run_command(self.shell_id, cmd, args)
  File "/usr/local/lib/python3.10/dist-packages/winrm/protocol.py", line 359, in run_command
    res = self.send_message(xmltodict.unparse(req))
  File "/usr/local/lib/python3.10/dist-packages/xmltodict.py", line 506, in unparse
    _emit(key, value, content_handler, full_document=full_document,
  File "/usr/local/lib/python3.10/dist-packages/xmltodict.py", line 462, in _emit
    _emit(child_key, child_value, content_handler,
  File "/usr/local/lib/python3.10/dist-packages/xmltodict.py", line 462, in _emit
    _emit(child_key, child_value, content_handler,
  File "/usr/local/lib/python3.10/dist-packages/xmltodict.py", line 462, in _emit
    _emit(child_key, child_value, content_handler,
  File "/usr/local/lib/python3.10/dist-packages/xmltodict.py", line 468, in _emit
    content_handler.characters(cdata)
  File "/usr/lib/python3.10/xml/sax/saxutils.py", line 212, in characters
    content = str(content, self._encoding)
TypeError: decoding to str: need a bytes-like object, list found

```

using workaround based on [stackoverflow](https://stackoverflow.com/questions/48376680/how-to-upload-a-file-to-windows-machine-using-winrmpython):

```sh
cat ~/test.ps1
```
```text
get-process
```
```sh
base64 ~/test.ps1
```
```text
Z2V0LXByb2Nlc3MKCg==
```
```python
>>> import winrm
>>> session = winrm.Session('192.168.0.185', auth = ('administrator', 'NP730qfg'))
>>>
>>> ps_script = """
 $s = 'Z2V0LXByb2Nlc3MKCg==';
$filepath = "${env:TEMP}\\test.ps1"
$data = [System.Convert]::FromBase64String($s);
add-content -value $data -encoding byte -path $filePath
. $filePath
"""

>>> result = session.run_ps(ps_script)
>>> result.status_code
0
>>>
result.status_code
```
```text
0
```
```python
>>> print(result.std_out.decode('utf8'))
```
```text
Handles  NPM(K)    PM(K)      WS(K)     CPU(s)     Id  SI ProcessName
-------  ------    -----      -----     ------     --  -- -----------
     88       6      864       4592       0.02   1828   0 AggregatorHost
     74       5     3252       3852       0.00   1008   0 cmd
    134      10     6504      12424       0.05   3472   0 conhost
    254      14     6592      19860       0.22   3680   1 conhost
    290      14     1744       5952       0.28    376   0 csrss
    258      13     1728       6024       0.17    452   1 csrss
    388      15     3340      19556       0.14   1748   1 ctfmon
    743      34    26540      56376       0.73    852   1 dwm
   1549      59    23308      84916       1.84   1880   1 explorer
     33       6     1308       3460       0.05    656   0 fontdrvhost
     33       6     1540       4036       0.08    664   1 fontdrvhost
      0       0       60          8                 0   0 Idle
    928      22     5008      15124       1.34    572   0 lsass
    227      13     2808      10364       0.08   2712   0 msdtc
    636      67   160688     167996      28.48   1760   0 MsMpEng
    208      11     3564      11108       0.14   2228   0 NisSrv
    257      15     2692      18260       0.13   1056   1 notepad
    569      46    41312      57696       4.55   2300   0 powershell
    574      51    44848      70992       5.36   3672   1 powershell
      0       7      824      70124       0.33     76   0 Registry
    254      14     3468      21668       0.16    676   1 RuntimeBroker
    270      15     4624      19788       0.23   3212   1 RuntimeBroker
    215      12     2060      12644       0.09   3452   1 RuntimeBroker
    636      32    30392      72608       0.84   3136   1 SearchApp
    299      11     3000       7528       1.03    564   0 services
    487      16     4760      26092       0.33   2432   1 sihost
     57       3     1084       1296       0.11    280   0 smss
    445      21     5284      16288       0.16   1568   0 spoolsv
    571      27    17524      61196       0.89   2408   1 StartMenuExperienceHost
    535      30    10116      23608       0.38    248   0 svchost
    589      26     5916      19772       0.50    628   0 svchost
    855      24     7040      28560       0.81    680   0 svchost
    702      17     4128      10520       1.19    760   0 svchost
   1228      43    20168      48784       9.75    964   0 svchost
    333      16    10996      14604       5.41    976   0 svchost
    576      21    15720      27952       1.84    988   0 svchost
    777      59     8184      22936       1.23   1236   0 svchost
    273      11     1784       8716       0.05   1256   0 svchost
    398      32     7608      17020       0.38   1292   0 svchost
    503      22    12640      28048       1.05   1600   0 svchost
    192      11     2092       8600       0.06   1668   0 svchost
    195      12     1588       7480       0.14   1740   0 svchost
    190      11     2396      15272       0.05   1960   1 svchost
    458      21     7132      33272       0.44   2396   1 svchost
    180      10     4812      13380       0.73   2532   0 svchost
    118       9     1544       6920       0.02   3120   0 svchost
   1451       0       40        144       2.88      4   0 System
    172      11     1984      11024       0.03   2788   1 taskhostw
    536      24    10068      43368       0.27   2180   1 TextInputHost
    152      11     1300       7000       0.13    444   0 wininit
    251      12     2388      11508       0.08    500   1 winlogon
    120       8     1580       6284       0.02   1212   0 winrshost
     58       5      708       3540       0.00   1780   0 wlms
```

![Windows Target](https://github.com/sergueik/springboot_study/blob/master/basic-harness/screenshots/capture-winrm-client.png)

### Delegate

```sh
cd delegate
export IMAGE=harness_delegate
docker build -t $IMAGE -f Dockerfile .
```
### See Also

  * [deploy to physical data center](https://developer.harness.io/docs/continuous-delivery/get-started/cd-tutorials/pdc) using a `harness/delegate` Docker instance
  * [deploy to Azure VM](https://developer.harness.io/docs/continuous-delivery/get-started/cd-tutorials/azure)
  * [Harness Community Edition](https://developer.harness.io/docs/continuous-delivery/deploy-srv-diff-platforms/community-ed/harness-community-edition-quickstart) [repo](https://github.com/harness/harness-cd-community/tree/main) - retired in favour of [Gitness](https://gitness.com/)
  * [Harness Community Edition deployments](https://developer.harness.io/docs/continuous-delivery/deploy-srv-diff-platforms/community-ed/harness-community-edition-quickstart)
  * [Harness Delegate overview](https://developer.harness.io/docs/platform/delegates/delegate-concepts/delegate-overview) - NOTE, Harness recommends that you create a custom image.
  * running  in Docker compose [repo](https://github.com/harness/harness-cd-community) and [docker-compose.yml](https://github.com/harness/harness-cd-community/blob/main/docker-compose/harness/docker-compose.yml)  using `harness/nextgenui-signed` ,`harness/ng-auth-ui-signed` ,`harness/ng-manager-signed` ,`harness/pipeline-service-signed` ,`harness/platform-service-signed` , `harness/log-service-signed:release-70-ubi`, `harness/ci-scm-signed`, `harness/delegate-proxy-signed`, `harness/nginx`,`harness/redis`, and `mongo`
  * [Harness Shell Script step example](https://developer.harness.io/docs/continuous-delivery/x-platform-cd-features/cd-steps/utilities/shell-script-step)
  * https://hostadvice.com/how-to/web-hosting/ubuntu/how-to-configure-firewall-with-ufw-on-ubuntu-18/


  * https://medium.com/@raghavmnnit/setting-up-winrm-communication-between-linux-and-windows-using-python-and-pywinrm-88f47a68bf7d (requires creation of account to access the material)
  * https://adamtheautomator.com/python-winrm/
  * https://www.google.com/search?q=python%20pywinrm%20tutorial
  * https://adamtheautomator.com/python-winrm/
  * https://pypi.org/project/pywinrm
  * https://pypi.org/project/winrmcp/
  * https://github.com/packer-community/winrmcp/tree/master/winrmcp Golang?
  * https://github.com/diyan/pywinrm/blob/f796b5aa15f0ce6c3e16aa3fd33a13efedff4937/winrm/__init__.py#L30
  * https://stackoverflow.com/questions/48376680/how-to-upload-a-file-to-windows-machine-using-winrmpython
  * https://github.com/rundeck-plugins/py-winrm-plugin

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
