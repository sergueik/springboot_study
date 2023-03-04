###  Info
This directory contains a replica of [example code](https://github.com/sayadi/elastic-apm-spring-boot-integration) from [article](https://levelup.gitconnected.com/how-to-integrate-elastic-apm-java-agent-with-spring-boot-7ce8388a206e) on adding the
[self-attached APM agent](https://www.elastic.co/guide/en/apm/agent/java/master/setup-attach-api.html)
to a java Spring application via dependency injection and conditional method run.

The key is to perform a static method invocation:
```java
Map<String, String> apmProps = new HashMap<>();
apmProps.put("server_url", server_url);
apmProps.put("service_name", service_name);
apmProps.put("secret_token", secret_token);
apmProps.put("environment", environment);
apmProps.put("application_packages", application_packages);
apmProps.put("log_level", log_level);

ElasticApmAttacher.attach(apmProps);
```
where the APM configuration strings `server_url`,`service_name`,`secret_token`, `environment`, `application_packages` and `log_level` come from `application.properties` (the property name prefix is defined in annotation):
```java
# Elastic APM
elastic.apm.enabled=true
elastic.apm.server_url=http://localhost:8200
elastic.apm.service_name=elastic_apm_spring_boot_integration
elastic.apm.secret_token=xxVpmQB2HMzCL9PgBHVrnxjNXXw5J7bd79DFm6sjBJR5HPXDhcF8MSb3vv4bpg44
elastic.apm.environment=dev
elastic.apm.application_packages=example
elastic.apm.log_level=DEBUG
```

### Usage

* collect base images
```sh
docker pull docker.elastic.co/elasticsearch/elasticsearch:7.7.1
docker pull docker.elastic.co/kibana/kibana:7.7.1
docker pull docker.elastic.co/apm/apm-server:7.7.1
```
* run elastic
```sh
 docker-compose up --build
```
* NOTE: the warning 
```text
basic-apm-agent-attach-kibana-1         | {"type":"log","@timestamp":"2023-03-03T23:10:39Z","tags":["warning","plugins-discovery"],"pid":7,"message":"Expect plugin \"id\" in camelCase, but found: triggers_actions_ui"}
```

can be ignored: the  cluster reports healthy status:
```sh
docker-compose ps
```
```text
NAME                                     IMAGE                                                  COMMAND                  SERVICE             CREATED             STATUS                   PORTS
basic-apm-agent-attach-apm-server-1      docker.elastic.co/apm/apm-server:7.17.7                "/usr/bin/tini -- /u…"   apm-server          3 minutes ago       Up About a minute        0.0.0.0:8200->8200/tcp, :::8200->8200/tcp
basic-apm-agent-attach-elasticsearch-1   docker.elastic.co/elasticsearch/elasticsearch:7.17.7   "/bin/tini -- /usr/l…"   elasticsearch       3 minutes ago       Up 3 minutes (healthy)   0.0.0.0:9200->9200/tcp, :::9200->9200/tcp, 9300/tcp
basic-apm-agent-attach-kibana-1          docker.elastic.co/kibana/kibana:7.17.7                 "/bin/tini -- /usr/l…"   kibana              3 minutes ago       Up 3 minutes (healthy)   0.0.0.0:5601->5601/tcp, :::5601->5601/tcp


```
the repetition of the command (after a small delay)

```text
docker-compose up --build
```
solves the health check error

```text
container for service "kibana" is unhealthy
```
No more information is available about this error


* run app "in dev environment"
```sh
 mvn spring-boot:run -Dspring-boot.run.profiles=dev
```
* test the `dev` environment

```sh
DEV_PORT=8082
curl http://192.168.0.92:$DEV_PORT/slow
```

#### Inspect APM

* After the ELK cluster is operational you still need to visit the 'Add APM' page `http://localhost:5601/app/kibana#/home/tutorial/apm` and 
click the `Check APM Server Status` button
![Check APM Server](https://github.com/serghttp://192.168.0.92:5601/app/observability/landingueik/springboot_study/blob/master/basic-apm-agent-attach/screenshots/capture-apm-server-status.png)

`Check APM Agent Status` button
![Check APM Agent](https://github.com/sergueik/springboot_study/blob/master/basic-apm-agent-attach/screenshots/capture-apm-agent-status.png)

and `Load Kibana Objects` button
![Load Kibana Objects](https://github.com/sergueik/springboot_study/blob/master/basic-apm-agent-attach/screenshots/capture-load-kibana-objects.png)

(the need in manual activation may be specific to relaively ond ELK release used in this project)

all three calls will report the success:


Action | Result
 --- | --- 
Server Status | You have correctly setup APM Server
Agent Status | Data successfully received from one or more agents
Kibana Objects | 1 saved objects successfully added

Note, with the build `7.17.7` the `Check Agent Status` button on `Observability` page `http://192.168.0.92:5601/app/observability/landing` is actually showing the warning message 
```text
No data has been received from agents yet
```

but the APM and transactions work fine
After that you will be able to successfully find the service `elastic-apm-spring-boot-integration` and the transaction 
`TestController#getSlowApi`
Since there is no distributed calls there will not be  anything of interest at that screen and there  will not be any `traceid` in the metadata

#### Side by Side APM

* run `predev` environment
```sh
mvn spring-boot:run -Dspring-boot.run.profiles=predev
```

* test the `predev` environment. 
```sh
PREDEV_PORT=8081
curl http://192.168.0.92:$PREDEV_PORT/fast
```
There should be no new APM information collected from `predev` environment

* test `dev` environment again with a different API

```sh
DEV_PORT=8082
curl http://192.168.0.92:$DEV_PORT/fast
```
```sh
DEV_PORT=8082
curl http://192.168.0.92:$DEV_PORT/slow

you will see the second REST API as a new transaction

![Transactions](https://github.com/sergueik/springboot_study/blob/master/basic-apm-agent-attach/screenshots/capture-two-transactions.png)



### See Also


   * general Spring `@Conditional` Annotation [documentation](https://www.baeldung.com/spring-conditionalonproperty)
   * Spring `@ConditionalOnProperty` Annotation [documentation](https://www.baeldung.com/spring-conditional-annotations)




  



### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
