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
``,

come from `application.properties`
```java
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
* run app
```sh
 mvn spring-boot:run -Dspring-boot.run.profiles=dev
```
* test `dev` environment

```sh
curl http://192.168.0.92:8082/slow
```
* inspect APM

### See Also


   * general Spring `@Conditional` Annotation [documentation](https://www.baeldung.com/spring-conditionalonproperty)
   * Spring `@ConditionalOnProperty` Annotation [documentation](https://www.baeldung.com/spring-conditional-annotations)




  



### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
