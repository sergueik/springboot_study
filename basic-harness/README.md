### Info

Replica of [harness-cd-community](https://github.com/harness/harness-cd-community) 
- a retired [Harness CD]() Community Edition a modern self-service continuous
delivery solution that allows to deploy, verify and automatically rollback Kubernetes and other cloud-native applications on any public or private cloud infrastructure of their choice.

> NOTE: The [Harness Community Edition deployments](https://developer.harness.io/docs/continuous-delivery/deploy-srv-diff-platforms/community-ed/harness-community-edition-quickstart/)  states that 
__Harness CD CE__ is deprecated. There is also a [Harness CD Community Edition overview](https://developer.harness.io/docs/continuous-delivery/deploy-srv-diff-platforms/community-ed/harness-community-edition-overview/)

Harnes CICD is a heavy 12+ node cluster

![Windows Target](https://github.com/sergueik/springboot_study/blob/master/basic-harness/screenshots/capture-cluster-docker-webgui.png)

with complex workflows:

![Harness Flow](https://github.com/sergueik/springboot_study/blob/master/basic-harness/screenshots/harness_ui_fanout.png)

### Usage

Check the disk space - you will need at minimum 6.5 GB for images and containers alone

```sh
df -h /
```
```text
Filesystem      Size  Used Avail Use% Mounted on
/dev/sda1        22G   14G  6.8G  68% /
```
* pull recent releases of Harness Docker image set

```sh
FILE=docker-compose.yml.792xx
FILE=docker-compose.yml.794xx
FILE=docker-compose.yml

grep image: $FILE | tr -d '\r' |cut -f 2,3 -d ':' | xargs -IX docker pull X
```
> NOTE: on Linux host one may prefer a different command variation:
```sh

FILE=docker-compose.yml.792xx
FILE=docker-compose.yml.794xx
FILE=docker-compose.yml
awk '/image:/ {print $2}' $FILE| xargs -IX docker pull X
```
examine the images
```sh
docker image ls
```
```text
harness/nextgenui-signed          0.353.10             288cf8d1df92        2 years ago         258MB
mongo                             4.4.22               cb026d11ad72        2 years ago         432MB
harness/ng-manager-signed         79421                dab3fa214c6f        2 years ago         1.04GB
harness/manager-signed            79421                577bd146fc5d        2 years ago         1.02GB
harness/platform-service-signed   79202                d92a76ceca5d        2 years ago         867MB
harness/pipeline-service-signed   1.33.8               7a582519dc37        2 years ago         871MB
harness/delegate-proxy-signed     79310                c1852cfaa23a        2 years ago         1.87GB
harness/log-service-signed        release-70-ubi       45c5a9b3c245        2 years ago         465MB
harness/ci-scm-signed             release-150-ubi      c309ed359c07        2 years ago         610MB
harness/ng-auth-ui-signed         1.7.0                6b42903695fa        2 years ago         176MB
harness/redis                     6.2.7-alpine         b6e4ce5f89f4        3 years ago         25.5MB
harness/nginx                     1.21.4               ea335eea17ab        4 years ago         141MB
harness/delegate                  24.09.83905.minimal  e5b511e65dbf        15 months ago       732MB
```
> NOTE: the 732 MB `harness/delegate` *is* the *minimal* Alpine-based image. Technically it is not a part of the cluster

```sh
docker-compose up --build --detach
```

```text
 - Container basic-harness-ng-manager-1        Started                     5.3s
 - Container basic-harness-scm-1               Started                     5.3s
 - Container basic-harness-log-service-1       Started                     5.2s
 - Container basic-harness-ng-auth-ui-1        Started                     5.3s
 - Container basic-harness-redis-1             Started                     5.2s
 - Container basic-harness-platform-service-1  Running                     0.0s
 - Container basic-harness-pipeline-service-1  Running                     0.0s
 - Container basic-harness-ng-ui-1             Running                     0.0s
 - Container basic-harness-mongo-1             Running                     0.0s
 - Container basic-harness-delegate-proxy-1    Running                     0.0s
 - Container basic-harness-manager-1           Running                     0.0s
 - Container basic-harness-proxy-1             Started                     5.4s
```
Examine the cluster status. 

```sh
docker-compose ps
```


There is probably dependencies across the nodes not configured in `docker-compsoe.yml` and the state of several nodes is 
seen in particular in Windows environment on __Docker Toolbox__, but also on Ubuntu with older releases of the project

```text
NAME                               COMMAND                  SERVICE             STATUS                PORTS
basic-harness-delegate-proxy-1     "/bin/sh -c 'nginx -…"   delegate-proxy      running (healthy)     8080/tcp
basic-harness-log-service-1        "/usr/local/bin/log-…"   log-service         running (healthy)     8079/tcp
basic-harness-manager-1            "./run.sh"               manager             running (unhealthy)   9090/tcp, 9879/tcp
basic-harness-mongo-1              "docker-entrypoint.s…"   mongo               running (healthy)     27017/tcp
basic-harness-ng-auth-ui-1         "/bin/sh -c 'sed -i …"   ng-auth-ui          running (healthy)     8080/tcp
basic-harness-ng-manager-1         "./run.sh"               ng-manager          running (unhealthy)   7090/tcp
basic-harness-ng-ui-1              "sh /opt/entrypoint.…"   ng-ui               running (healthy)     8080/tcp
basic-harness-pipeline-service-1   "/opt/harness/run.sh"    pipeline-service    running (unhealthy)   12001/tcp, 12011/tcp, 14002/tcp
basic-harness-platform-service-1   "/opt/harness/run.sh"    platform-service    running (healthy)     9005/tcp
basic-harness-proxy-1              "/docker-entrypoint.…"   proxy               running (healthy)     0.0.0.0:80->80/tcp, 0.0.0.0:9879->9879/tcp
basic-harness-redis-1              "docker-entrypoint.s…"   redis               running (healthy)     6379/tcp
basic-harness-scm-1                "/usr/local/bin/scm"     scm                 running (healthy)     8091/tcp
```

try to restart the unhealthy nodes - note the naming conventions

```sh
docker-compose up pipeline-service --detach
```
```sh
docker-compose up ng-manager --detach
```
```sh
docker-compose up manager --build --detach
```
```sh
curl -sv http://$(docker-machine ip):12001/api/health
```
```text
* Uses proxy env variable no_proxy == '192.168.99.100,192.168.99.103,192.168.99.104,192.168.99.102'
*   Trying 192.168.99.100:12001...
* connect to 192.168.99.100 port 12001 from 0.0.0.0 port 60233 failed: Connection refused
* Failed to connect to 192.168.99.100 port 12001 after 2016 ms: Could not connect to server
* closing connection #0
```

```sh
docker-compose logs manager | tail -30
```
```text
basic-harness-manager-1  | 2025-12-30 21:47:08,033 [main] ERROR io.harness.timescaledb.retention.RetentionManagerImpl - Error while adding retention policy 7 months for timescale table instance_stats_hour
basic-harness-manager-1  | java.sql.SQLException: Invalid timescale db.
basic-harness-manager-1  |      at io.harness.timescaledb.TimeScaleDBServiceImpl.getDBConnection(TimeScaleDBServiceImpl.java:145)
basic-harness-manager-1  |      at io.harness.timescaledb.retention.RetentionManagerImpl.addPolicyInternal(RetentionManagerImpl.java:55)
basic-harness-manager-1  |      at io.harness.timescaledb.retention.RetentionManagerImpl.addPolicy(RetentionManagerImpl.java:49)
basic-harness-manager-1  |      at io.harness.cd.timescale.CDRetentionHandler.configureRetentionPolicy(CDRetentionHandler.java:36)
basic-harness-manager-1  |      at software.wings.app.WingsApplication.run(WingsApplication.java:511)
basic-harness-manager-1  |      at software.wings.app.WingsApplication.run(WingsApplication.java:397)
basic-harness-manager-1  |      at io.dropwizard.cli.EnvironmentCommand.run(EnvironmentCommand.java:59)
basic-harness-manager-1  |      at io.dropwizard.cli.ConfiguredCommand.run(ConfiguredCommand.java:98)
basic-harness-manager-1  |      at io.dropwizard.cli.Cli.run(Cli.java:78)
basic-harness-manager-1  |      at io.dropwizard.Application.run(Application.java:94)
basic-harness-manager-1  |      at software.wings.app.WingsApplication.main(WingsApplication.java:422)
basic-harness-manager-1  | ./start_process.sh: line 99:   189 Killed                  java $JAVA_OPTS -jar $CAPSULE_JAR $COMMAND /opt/harness/config.yml

```
```sh
docker-compose exec manager sh -c 'cat /opt/harness/start_process.sh' | tee a.txt
sed -n '97,101p' a.txt
```
```text 
        java $JAVA_OPTS -jar $CAPSULE_JAR $COMMAND /opt/harness/config.yml > /opt/harness/logs/portal.log 2>&1
    fi
fi
```

the happy status on Ubuntu Docker
 
```sh
docker-compose ps
```
```txt
               Name                            Command                  State                       Ports                 
-------------------------------------------------------------------------------------------------------------------------
basic-harness_delegate-proxy_1     /bin/sh -c nginx -c "/etc/ ...   Up (healthy)   8080/tcp                              
basic-harness_log-service_1        /usr/local/bin/log-service ...   Up (healthy)   8079/tcp                              
basic-harness_manager_1            ./run.sh                         Up (healthy)   9090/tcp, 9879/tcp                    
basic-harness_mongo_1              docker-entrypoint.sh --wir ...   Up (healthy)   27017/tcp                             
basic-harness_ng-auth-ui_1         /bin/sh -c sed -i "s|<\!-- ...   Up (healthy)   8080/tcp                              
basic-harness_ng-manager_1         ./run.sh                         Up (healthy)   7090/tcp                              
basic-harness_ng-ui_1              sh /opt/entrypoint.sh            Up (healthy)   8080/tcp                              
basic-harness_pipeline-service_1   /opt/harness/run.sh              Up (healthy)   12001/tcp, 12011/tcp, 14002/tcp       
basic-harness_platform-service_1   /opt/harness/run.sh              Up (healthy)   9005/tcp                              
basic-harness_proxy_1              /docker-entrypoint.sh ngin ...   Up (healthy)   0.0.0.0:80->80/tcp,:::80->80/tcp, 0.0.
                                                                                   0.0:9879->9879/tcp,:::9879->9879/tcp  
basic-harness_redis_1              docker-entrypoint.sh redis ...   Up (healthy)   6379/tcp                              
basic-harness_scm_1                /usr/local/bin/scm               Up (healthy)   8091/tcp          
```

### Create User
navigate to `http://localhost/#/signup`, create a user

![Harness Signup](https://github.com/sergueik/springboot_study/blob/master/basic-harness/screenshots/capture-harness-signup.png)

> NOTE: the password will not authenticate you anywhere but this cluster


### Setting Org and Project

The default organization will be used, but the project will need to be created explicitly

![Harness Login](https://github.com/sergueik/springboot_study/blob/master/basic-harness/screenshots/capture-harness-login.png)

__Harness__ will prompt to set up pipeline 


![Harness Pipeline](https://github.com/sergueik/springboot_study/blob/master/basic-harness/screenshots/capture-quickstart-pipeline.png)

To run pipeline one is prompted to create a delegate first

![Harness Install Delegate](https://github.com/sergueik/springboot_study/blob/master/basic-harness/screenshots/capture-install-new-delegate.png)



![Harness No Delegate Found Screen](https://github.com/sergueik/springboot_study/blob/master/basic-harness/screenshots/capture-need-new-delegate.png)

Error with delegate configuration:

![Harness Delegate Setup Error](https://github.com/sergueik/springboot_study/blob/master/basic-harness/screenshots/capture-delegate-setup-error.png)

debugging the error in browser 

```text
GET /api/setup/delegates/installation-command
  ?routingId=ZcCMqDV4QL2-jmv-bHn5ow
  &accountId=ZcCMqDV4QL2-jmv-bHn5ow
  &commandType=DOCKER
500 Internal Server Error
```
indicates misconfiguration. 

Delegate lifecycle is out-of-band - installing one is the first UI action that requires a fully valid routing context. 
The `routingId`, `accountid` arguments passed cannot be identical.

![Harness Delegate Root Cause](https://github.com/sergueik/springboot_study/blob/master/basic-harness/screenshots/capture-error-routingId.png)

The `routingId` does not apppear to be saved in the session

This defect was observed in two most recent releases of __Harness CD CE__.

Errors have been observed in other actions too:

![Harness Generic Error Page](https://github.com/sergueik/springboot_study/blob/master/basic-harness/screenshots/capture-generic-error.png)

![Harness Delegate Root Cause](https://github.com/sergueik/springboot_study/blob/master/basic-harness/screenshots/capture-custom-error.png)

Some errors are reported after time consuming attempting to set up the environment:


![Harness Delegate Root Cause](https://github.com/sergueik/springboot_study/blob/master/basic-harness/screenshots/capture-intrastructure-provisioning-underway.png)


![Harness Delegate Root Cause](https://github.com/sergueik/springboot_study/blob/master/basic-harness/screenshots/capture-intrastructure-provisioning-failed.png)

this may be related to

```sh
docker-compose logs  ng-manager | grep ERROR |grep io.harness.mongo | head -3
```
```text
WARNING: Python-dotenv could not parse statement starting at line 17
ng-manager_1        | 2025-12-31 15:58:12,062 [main] ERROR io.harness.mongo.IndexManagerSession - Index {"name": "accountIdentifier_1", "background": true} is a subsequence of index {"name": "unique_accountIdentifier_userIdentifier_type_idx", "unique": true} [collectionName=userSourceCodeManagers] 
ng-manager_1        | 2025-12-31 15:58:12,351 [main] ERROR io.harness.mongo.IndexManagerSession - Index {"name": "accountId_organizationId_projectId_lastDeployedAt_idx", "background": true} is a subsequence of index {"name": "accountId_organizationId_projectId_lastDeployedAt_serviceIdentifier_idx", "background": true} [collectionName=instanceNG] 
ng-manager_1        | 2025-12-31 15:58:14,047 [main] ERROR io.harness.mongo.IndexManagerSession - Index {"name": "unique_accountIdentifier_organizationIdentifier_projectIdentifier", "unique": true, "collation": {"locale": "en", "strength": 1}} is a subsequence of index {"name": "accountOrgIdentifierDeletedCreatedAtLastModifiedAtIdx", "background": true} [collectionName=projects] 
```


### Harness Docker Toolbox / VirtualBox / Hypervisor-layer Pathology

Harness self-bootstrap instability is an expected failure mode, 
given the number of moving parts below Docker (VT-x, nested virt, NAT timing, clock skew, disk I/O)

Adding a self-health gate to the repo

* some nodes have no healthcheck
* some nodes (delegate) are allowed to fail or exit
* results vary across hypervisors


#### Sample Runs
```sh
docker-compose ps
```
```text
NAME                               COMMAND                  SERVICE             STATUS                PORTS
basic-harness-delegate-1           "./start.sh"             delegate            exited (1)
basic-harness-delegate-proxy-1     "/bin/sh -c 'nginx -…"   delegate-proxy      running (healthy)     8080/tcp
basic-harness-log-service-1        "/usr/local/bin/log-…"   log-service         running (healthy)     8079/tcp
basic-harness-manager-1            "./run.sh"               manager             exited (137)
basic-harness-mongo-1              "docker-entrypoint.s…"   mongo               running (healthy)     27017/tcp
basic-harness-ng-auth-ui-1         "/bin/sh -c 'sed -i …"   ng-auth-ui          running (healthy)     8080/tcp
basic-harness-ng-manager-1         "./run.sh"               ng-manager          running (unhealthy)   7090/tcp
basic-harness-ng-ui-1              "sh /opt/entrypoint.…"   ng-ui               running (healthy)     8080/tcp
basic-harness-pipeline-service-1   "/opt/harness/run.sh"    pipeline-service    running (healthy)     12001/tcp, 12011/tcp, 14002/tcp
basic-harness-platform-service-1   "/opt/harness/run.sh"    platform-service    running (healthy)     9005/tcp
basic-harness-proxy-1              "/docker-entrypoint.…"   proxy               running (healthy)     0.0.0.0:80->80/tcp, 0.0.0.0:9879->9879/tcp
basic-harness-redis-1              "docker-entrypoint.s…"   redis               running (healthy)     6379/tcp
basic-harness-scm-1                "/usr/local/bin/scm"     scm                 running (healthy)     8091/tcpNAME                               COMMAND                  SERVICE             STATUS                PORTS
basic-harness-delegate-1           "./start.sh"             delegate            exited (1)
basic-harness-delegate-proxy-1     "/bin/sh -c 'nginx -…"   delegate-proxy      running (healthy)     8080/tcp
basic-harness-log-service-1        "/usr/local/bin/log-…"   log-service         running (healthy)     8079/tcp
basic-harness-manager-1            "./run.sh"               manager             exited (137)
basic-harness-mongo-1              "docker-entrypoint.s…"   mongo               running (healthy)     27017/tcp
basic-harness-ng-auth-ui-1         "/bin/sh -c 'sed -i …"   ng-auth-ui          running (healthy)     8080/tcp
basic-harness-ng-manager-1         "./run.sh"               ng-manager          running (unhealthy)   7090/tcp
basic-harness-ng-ui-1              "sh /opt/entrypoint.…"   ng-ui               running (healthy)     8080/tcp
basic-harness-pipeline-service-1   "/opt/harness/run.sh"    pipeline-service    running (healthy)     12001/tcp, 12011/tcp, 14002/tcp
basic-harness-platform-service-1   "/opt/harness/run.sh"    platform-service    running (healthy)     9005/tcp
basic-harness-proxy-1              "/docker-entrypoint.…"   proxy               running (healthy)     0.0.0.0:80->80/tcp, 0.0.0.0:9879->9879/tcp
basic-harness-redis-1              "docker-entrypoint.s…"   redis               running (healthy)     6379/tcp
basic-harness-scm-1                "/usr/local/bin/scm"     scm                 running (healthy)     8091/tcp
```

```sh
./wait_harness.sh
```

```text
Running Harness self-health check...
❌ ERROR: non-delegate container exited
/basic-harness-manager-1: exited (137)
```
as usual one checks logs:
```sh
docker-compose logs ng-manager
```
and after discovering a communication error
```text
basic-harness-ng-manager-1  | 2026-01-05 20:41:25,693 [FreezeTemplateRegistrationService-0] WARN  io.harness.remote.client.NGRestUtils - Request failed. Attempt : 2.
basic-harness-ng-manager-1  | java.net.ConnectException: Failed to connect to localhost/127.0.0.1:9005
...
basic-harness-ng-manager-1  | Caused by: java.net.ConnectException: Connection refused (Connection refused)
basic-harness-ng-manager-1  |   at java.base/java.net.PlainSocketImpl.socketConnect(Native Method)
basic-harness-ng-manager-1  |   ... 41 common frames omitted
```
and a typical cure attempt is to simply restart the cluser hoping that the odds will be better this time

```sh
docker-compose restart
```
```text
 - Container basic-harness-redis-1             Started                                                     7.4s
 - Container basic-harness-pipeline-service-1  Started                                                    16.7s
 - Container basic-harness-ng-auth-ui-1        Started                                                     7.1s
 - Container basic-harness-delegate-proxy-1    Started                                                     6.9s
 - Container basic-harness-ng-ui-1             Started                                                    15.4s
 - Container basic-harness-mongo-1             Started                                                    16.9s
 - Container basic-harness-platform-service-1  Started                                                    16.2s
 - Container basic-harness-ng-manager-1        Started                                                    16.0s
 - Container basic-harness-log-service-1       Started                                                    15.1s
 - Container basic-harness-manager-1           Started                                                     6.5s
 - Container basic-harness-scm-1               Started                                                     6.8s
 - Container basic-harness-delegate-1          Started                                                     6.6s
 - Container basic-harness-proxy-1             Started                                                     2.3s
```
but
```sh
./wait_harness.sh
Running Harness self-health check...
⏳ Waiting: starting=12 unhealthy=0 exited=0 (5s)
⏳ Waiting: starting=12 unhealthy=0 exited=0 (10s)
...
⏳ Waiting: starting=4 unhealthy=0 exited=0 (275s)
⏳ Waiting: starting=4 unhealthy=0 exited=0 (280s)
❌ ERROR: unhealthy container detected
```

```sh
./wait_harness.sh --inspect
```
```text
Running Harness self-health check...
❌ FAILURE detected

Exited containers:
/basic-harness-manager-1: exited (code=137)

Unhealthy containers:
/basic-harness-manager-1: unhealthy

```

### Misc.

* Pipelines 
* Services
* Environments

* Chaos
* Feature Flags
* Builds

Expect a two digit load average on an 16 GB 4 core laptop and expect networking changes to be occuring while Harness cluster is running (inbound connections may be blocked)

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
is fixed through updating the configuration
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
and re-run

```sh
docker-compose up --build platform-service
```

### Cleanup

* check the disk usage 
```sh
df -h /
```
```text
```
```sh
ls -hl ~/.docker/machine/machines/default/disk.vmdk
```
```text
-rw-r--r-- 1 kouzm 197609 9.6G Dec 30 15:44 /c/Users/kouzm/.docker/machine/machines/default/disk.vmdk
```


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
winrm set winrm/config/service/auth '@{Basic="true"}
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
### Cleanup

Check disk usage
```sh
df  -h /
```
```text
Filesystem      Size  Used Avail Use% Mounted on
/dev/sda1        22G   20G  518M  98% /

```
```sh
docker-compose stop
docker-compose rm -f
docker system prune -f
docker volume prune -f
docker image ls |grep harness | awk '{print $3}' | cut -c1-4 | xargs -IX docker image rm X
docker image rm 7dd6 cb02
```
### Misc. Screenshots

![screenshot](https://github.com/sergueik/springboot_study/blob/master/basic-harness/screenshots/capture-account-resources.png)

![screenshot](https://github.com/sergueik/springboot_study/blob/master/basic-harness/screenshots/capture-add-change-source.png)

![screenshot](https://github.com/sergueik/springboot_study/blob/master/basic-harness/screenshots/capture-add-healthsource.png)

![screenshot](https://github.com/sergueik/springboot_study/blob/master/basic-harness/screenshots/capture-add-template.png)

![screenshot](https://github.com/sergueik/springboot_study/blob/master/basic-harness/screenshots/capture-bad-custom-error.png)

![screenshot](https://github.com/sergueik/springboot_study/blob/master/basic-harness/screenshots/capture-configure-notification.png)

![screenshot](https://github.com/sergueik/springboot_study/blob/master/basic-harness/screenshots/capture-create-source-code-mananer.png)

![screenshot](https://github.com/sergueik/springboot_study/blob/master/basic-harness/screenshots/capture-feature-flags.png)

![screenshot](https://github.com/sergueik/springboot_study/blob/master/basic-harness/screenshots/capture-modules1.png)

![screenshot](https://github.com/sergueik/springboot_study/blob/master/basic-harness/screenshots/capture-resource-groups-upgrade-required.png)

![screenshot](https://github.com/sergueik/springboot_study/blob/master/basic-harness/screenshots/capture-targets.png)

![screenshot](https://github.com/sergueik/springboot_study/blob/master/basic-harness/screenshots/capture-trial-security-testing.png)

![screenshot](https://github.com/sergueik/springboot_study/blob/master/basic-harness/screenshots/capture-wizard-driven-start.png)

### Note 

[MiniKube](https://minikube.sigs.k8s.io/docs/start/?arch=%2Fwindows%2Fx86-64%2Fstable%2Fwindows+package+manager) 
is container / virtual machine manager-agnistic accepts one of: Docker, QEMU, Hyperkit, Hyper-V, KVM, Parallels, Podman, VirtualBox, or VMware Fusion/Workstation
VirtualBox  is MiniKube's original driver

```sh
curl -skLo ~/Downloads/minikube-installer.exe  https://github.com/kubernetes/minikube/releases/download/v1.20.0/minikube-installer.exe
```
```cmd
cd %USERPROFILE%\Downloads
.\minikube-installer.exe
```
> NOTE: you do not *have to* choose `C:\Minikube` as installation folder
```cmd
minikube.exe start --driver=virtualbox
```
You may experience warning in the console:
```text
* kubectl not found. If you need it, try: 'minikube kubectl -- get pods -A'
```
after this command is run
```text
NAMESPACE     NAME                               READY   STATUS    RESTARTS   AGE
kube-system   coredns-78fcd69978-6pspt           1/1     Running   0          2m29s
kube-system   etcd-minikube                      1/1     Running   0          2m36s
kube-system   kube-apiserver-minikube            1/1     Running   0          2m39s
kube-system   kube-controller-manager-minikube   1/1     Running   0          2m39s
kube-system   kube-proxy-5z998                   1/1     Running   0          2m29s
kube-system   kube-scheduler-minikube            1/1     Running   0          2m36s
kube-system   storage-provisioner                1/1     Running   0          2m34s
```
but for Harness may not be sufficient

for guaranteed cleanup will likely call explicitly

```cmd
"c:\Program Files\Kubernetes\Minikube\uninstall.exe"
```
### See Also

  * [Continuous Delivery and GitOps - Getting Started](https://developer.harness.io/docs/category/get-started)
  * [overview](https://developer.harness.io/docs/continuous-integration/get-started/overview)
  * [CD pipeline modeling overview](https://developer.harness.io/docs/continuous-delivery/get-started/cd-pipeline-modeling-overview)
  * [deploy to physical data center](https://developer.harness.io/docs/continuous-delivery/get-started/cd-tutorials/pdc) using a `harness/delegate` Docker instance
  * [deploy to Azure VM](https://developer.harness.io/docs/continuous-delivery/get-started/cd-tutorials/azure)
  * [Harness Community Edition](https://developer.harness.io/docs/continuous-delivery/deploy-srv-diff-platforms/community-ed/harness-community-edition-quickstart) [repo](https://github.com/harness/harness-cd-community/tree/main) - retired in favour of [Gitness](https://gitness.com/)
  * [Harness Community Edition deployments](https://developer.harness.io/docs/continuous-delivery/deploy-srv-diff-platforms/community-ed/harness-community-edition-quickstart)
  * [Harness Delegate overview](https://developer.harness.io/docs/platform/delegates/delegate-concepts/delegate-overview) - NOTE, Harness recommends that you create a custom image.
  * running  in Docker compose [repo](https://github.com/harness/harness-cd-community) and [docker-compose.yml](https://github.com/harness/harness-cd-community/blob/main/docker-compose/harness/docker-compose.yml) using 
    + `harness/nextgenui-signed` 
    + `harness/ng-auth-ui-signed` 
    + `harness/ng-manager-signed` 
    + `harness/pipeline-service-signed`
    + `harness/platform-service-signed` 
    + `harness/log-service-signed:release-70-ubi`
    + `harness/ci-scm-signed`
    + `harness/delegate-proxy-signed`
    + `harness/nginx`
    + `harness/redis`
    +  `mongo`

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
