###  Info
This directory contains a replica of [example code](https://github.com/sayadi/elastic-apm-spring-boot-integration) 
from

[article](https://levelup.gitconnected.com/how-to-integrate-elastic-apm-java-agent-with-spring-boot-7ce8388a206e) on adding the

The key is to perform a static method invocation:
```java
Map<String, String> apmProps = new HashMap<>(6);
        apmProps.put(SERVER_URL_KEY, serverUrl);
        apmProps.put(SERVICE_NAME_KEY, serviceName);
        apmProps.put(SECRET_TOKEN_KEY, secretToken);
        apmProps.put(ENVIRONMENT_KEY, environment);
        apmProps.put(APPLICATION_PACKAGES_KEY, applicationPackages);
        apmProps.put(LOG_LEVEL_KEY, logLevel);

        ElasticApmAttacher.attach(apmProps);
```

where the APM configuration strings 
`server-url`,`service-name`,`secret-token`, `environment`, `application-packages` and `log-level`

come from `application.properties` (the prefix is defined in annotation):
```java
# Elastic APM
elastic.apm.enabled=true
elastic.apm.server-url=http://localhost:8200
elastic.apm.service-name=elastic-apm-spring-boot-integration
elastic.apm.secret-token=xxVpmQB2HMzCL9PgBHVrnxjNXXw5J7bd79DFm6sjBJR5HPXDhcF8MSb3vv4bpg44
elastic.apm.environment=dev
elastic.apm.application-packages=io.sayadi.elasticapmspringbootintegration
elastic.apm.log-level=DEBUG

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

can be ignored, the repetition of the command

```text
docker-compose up --build
```
solves the error

```text
container for service "kibana" is unhealthy
```
No more information is available


* run app
```sh
 mvn spring-boot:run -Dspring-boot.run.profiles=dev
```
* test `dev` environment

```sh
DEV_PORT=8082
curl http://192.168.0.92:$DEV_PORT/slow
```
#### Inspect APM

* After the ELK cluster is operational you still need to visit the '' page `http://localhost:5601/app/kibana#/home/tutorial/apm` and 
click the `Check APM Server Status` button
![Check APM Server](https://github.com/sergueik/springboot_study/blob/master/basic-apm-agent-attach/screenshots/capture-apm-server-status.png)

`Check APM Agent Status` button
![Check APM Agent](https://github.com/sergueik/springboot_study/blob/master/basic-apm-agent-attach/screenshots/capture-apm-agent-status.png)

and `Load Kibana Objects` button
![Load Kibana Objects](https://github.com/sergueik/springboot_study/blob/master/basic-apm-agent-attach/screenshots/capture-apm-server-status.png)

(this may be specific to relaively ond ELK release used in this project)
all three will report the success:
Action | Result
 --- | --- 
Server Status | You have correctly setup APM Server
Agent Status | Data successfully received from one or more agents
Kibana Objects | 1 saved objects successfully added



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
There should no be any new APM information collected


* test `dev` environment again with a different API

```sh
DEV_PORT=8082
curl http://192.168.0.92:$DEV_PORT/fast
```

you will see the second REST API as a new transaction

![Transactions](https://github.com/sergueik/springboot_study/blob/master/basic-apm-agent-attach/screenshots/capture-two-transactions.png)



### See Also


   * general Spring `@Conditional` Annotation [documentation](https://www.baeldung.com/spring-conditionalonproperty)
   * Spring `@ConditionalOnProperty` Annotation [documentation](https://www.baeldung.com/spring-conditional-annotations)




  



### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
