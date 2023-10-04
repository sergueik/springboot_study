#### Info

this directory contains replica of the skeleton
springboot vault-backed spring cloud configuration [example repository](https://github.com/codeforgeyt/vault-demo-mvn)
connected to the Alpine hosted HashiCorp Vault [running in Docker container](https://github.com/dweomer/dockerfiles-vault/blob/master/Dockerfile).

the steps to configure the Vault and `bootstrap.properties` are covered in [youtube video](https://youtu.be/MaTDiKp_IrA)

### Usage

* build package on host (NOTE: skip the tests)
```sh
mvn -Dmaven.test.skip=true package
```
* build the image with Java and Vault applications
```sh
export IMAGE=basic-vault
docker build --build-arg "GID=$(id -g)" --build-arg "UID=$(id -u)" -t $IMAGE -f Dockerfile .
```
* NOTE: on a Windows host the values returned by `id` will be big integer numbers like `197609`. On a Linux host you will posibly see numbers like `1000`

* run the container interactively, override the entry point, and exposing the default port, to execute vault commands in the foreground
```sh
NAME=vault-container
docker run --rm --name $NAME --entrypoint "" -p 8200:8200 -it $IMAGE sh
```
```sh
vault -version
```
```text
Vault v1.12.2 (415e1fe3118eebd5df6cb60d13defdc01aa17b03), built 2022-11-23T12:53:46Z
```
* start dev server with options
```sh
TOKEN=$(echo 'vault token' | base64 -)
echo $TOKEN
vault server -dev -dev-listen-address=0.0.0.0:8200 -dev-root-token-id=$TOKEN
```
this will print to console
```text
The unseal key and root token are displayed below in case you want to
seal/unseal the Vault or re-authenticate.

Unseal Key: t1Pz89JEGcc4p/tYrJYlKEFFVLbHQ4kD+diNMQ6f2LE=
Root Token: dmF1bHQgdG9rZW4K

Development mode should NOT be used in production installations!
```
and vault will remain running

If the key was changed extract it while running shell on the host:
```sh
sed -n 's/spring.cloud.vault.token=//p'  src/main/resources/bootstrap.properties
```
```text
dmF1bHQgdG9rZW4K
```
then in the container use the value
```sh
TOKEN=dmF1bHQgdG9rZW4K
echo $TOKEN
vault server -dev -dev-listen-address=0.0.0.0:8200 -dev-root-token-id=$TOKEN
```
* NOTE: a simple PLAIN strings like 'example' do not always work - but the problem will only be discovered in runtime through observed Java aplication failure

* make sure use the `spring.cloud.vault.token` value from `src/main/resources/bootstrap.properties`
this will allow connecting to the container through the ip address of the dev host (`http://localhost:8200/ui/` for Linux host, the address will be shown by `docker-machine ip` for Windows host, e.g. `http://192.168.99.100:8200/ui/`):

![Vault UI](https://github.com/sergueik/springboot_study/blob/master/basic-spring-vault/screenshots/capture-login.png)

alternatively can skip the `dev-root-token-id` option and

collect the randomly generated token information to continue
```text
Root Token: hvs.LhxlbD0lD1QWV33DFfsSlauA
```
update `src/main/resources/bootstrap.properties`, repackage, copy image into the running container
```sh
export ID=$(docker container ls -a| grep $IMAGE| awk '{print $1}')

docker cp target/example.basic-vault.jar $ID:/work/app.jar
```




to do this, run on development host

```sh
export VAULT_ADDR=http://127.0.0.1:8200/
sed -n 's/spring.cloud.vault.token=//p'  src/main/resources/bootstrap.properties
echo $TOKEN
export VAULT_TOKEN=$TOKEN
NAME=vault-container
docker exec -e VAULT_TOKEN=$TOKEN -e VAULT_ADDR=http://127.0.0.1:8200/ $NAME vault kv get secret/application
```
this wll print
```text
No value found at secret/data/application
```

NOTE:  without the settings, command will fail with complain
```text
Get "https://127.0.0.1:8200/v1/sys/internal/ui/mounts/secret/application": http: server gave HTTP response to HTTPS client
```

alternarively open the second interactive shell to the same container and execute commands there:
```sh
docker exec -it $NAME  sh
```

then in the second shell on the conainer:
```sh
export VAULT_ADDR=http://127.0.0.1:8200/
TOKEN=$(echo 'vault token' | base64 -)
echo $TOKEN
export VAULT_TOKEN=$TOKEN
vault kv get secret/application
```


* initilize the sensitive variables:
```sh
docker exec -e VAULT_TOKEN=$TOKEN -e VAULT_ADDR=http://127.0.0.1:8200/ $NAME vault kv put secret/application login=testuser password=test123
```

this will print
```text
===== Secret Path =====
secret/data/application

======= Metadata =======
Key                Value
---                -----
created_time       2023-10-04T15:54:45.289732869Z
custom_metadata    <nil>
deletion_time      n/a
destroyed          false
version            1
```

* do exercises from a second console

```sh
IMAGE=basic-vault
ID=$(docker container ls | grep $IMAGE |awk '{print $1}')
docker exec -it $ID sh
```
```sh
export VAULT_ADDR=http://127.0.0.1:8200/
TOKEN=$(echo 'vault token' | base64 -)
echo $TOKEN
export VAULT_TOKEN=$TOKEN
```
* put secret data into Vault
NOTE: combine both inputs in single command (may not be necessary)
```sh
vault kv put secret/application login=testuser password=test123
```
NOTE: the `secret` secret engine and the `application` path are the default ones Java application is configured to use through `bootstrap.properties`:

```java
spring.application.name=basic-vault
spring.cloud.vault.uri=http://localhost:8200
spring.cloud.vault.generic.backend=secret
spring.cloud.vault.generic.default-context=application
```

* verify the secret
```sh
vault kv get secret/application
```
this will print 

```text
===== Secret Path =====
secret/data/application

======= Metadata =======
Key                Value
---                -----
created_time       2023-07-06T01:38:37.055689815Z
custom_metadata    <nil>
deletion_time      n/a
destroyed          false
version            5

====== Data ======
Key         Value
---         -----
login       testuser
password    test123
```
* print the full JSON
```sh
vault kv get -format=json secret/application| jq -r '.'
```
this will print
```json

{
  "request_id": "1082b408-df32-2079-93b9-9e5691ac3769",
  "lease_id": "",
  "lease_duration": 0,
  "renewable": false,
  "data": {
    "data": {
      "login": "testuser",
      "password": "test123"
    },
    "metadata": {
      "created_time": "2023-07-06T01:38:37.055689815Z",
      "custom_metadata": null,
      "deletion_time": "",
      "destroyed": false,
      "version": 5
    }
  },
  "warnings": null
}
```
alternatively create and observe via UI:

![Configured Application Secrets](https://github.com/sergueik/springboot_study/blob/master/basic-spring-vault/screenshots/capture-secret.png)

![Secret Value Displayed](https://github.com/sergueik/springboot_study/blob/master/basic-spring-vault/screenshots/capture-secret2.png)

* run the Java app in Docker container console

```sh
java -jar app.jar
```

or from the development host
```sh
docker exec $NAME java -jar app.jar
```
this will print to console the starnard Spring banned and possibly few ignorable warnings, then print the sensitive data received from Vault:
```text
2023-10-04 16:04:19.334  INFO 64 --- [           main] o.s.s.c.ThreadPoolTaskScheduler          : Initializing ExecutorService

  .   ____          _            __ _ _
 /\\ / ___'_ __ _ _(_)_ __  __ _ \ \ \ \
( ( )\___ | '_ | '_| | '_ \/ _` | \ \ \ \
 \\/  ___)| |_)| | | | | || (_| |  ) ) ) )
  '  |____| .__|_| |_|_| |_\__, | / / / /
 =========|_|==============|___/=/_/_/_/
 :: Spring Boot ::        (v2.3.4.RELEASE)

2023-10-04 16:04:22.909  INFO 64 --- [           main] o.s.v.c.e.LeaseAwareVaultPropertySource  : Vault location [secret/basic-vault] not resolvable: Not found
2023-10-04 16:04:22.998  INFO 64 --- [           main] b.c.PropertySourceBootstrapConfiguration : Located property source: [BootstrapPropertySource {name='bootstrapProperties-secret/basic-vault'}, BootstrapPropertySource {name='bootstrapProperties-secret/application'}]
2023-10-04 16:04:23.086  INFO 64 --- [           main] c.c.c.ConfigServicePropertySourceLocator : Fetching config from server at : http://localhost:8888
2023-10-04 16:04:23.297  INFO 64 --- [           main] c.c.c.ConfigServicePropertySourceLocator : Connect Timeout Exception on Url - http://localhost:8888. Will be trying the next url if available
2023-10-04 16:04:23.306  WARN 64 --- [           main] c.c.c.ConfigServicePropertySourceLocator : Could not locate PropertySource: I/O error on GET request for"http://localhost:8888/basic-vault/default": Connection refused (Connection refused); nested exception is java.net.ConnectException: Connection refused (Connection refused)
2023-10-04 16:04:23.369  INFO 64 --- [           main] example.Application                      : No active profile set, falling back to default profiles: default
2023-10-04 16:04:29.348  INFO 64 --- [           main] o.s.cloud.context.scope.GenericScope     : BeanFactory id=02bc8d54-4cb5-3c3d-a568-d793f6fe7cfe
2023-10-04 16:04:32.044  INFO 64 --- [           main] o.s.b.w.embedded.tomcat.TomcatWebServer  : Tomcat initialized with port(s): 8085 (http)
2023-10-04 16:04:32.159  INFO 64 --- [           main] o.apache.catalina.core.StandardService   : Starting service [Tomcat]
2023-10-04 16:04:32.162  INFO 64 --- [           main] org.apache.catalina.core.StandardEngine  : Starting Servlet engine: [Apache Tomcat/9.0.38]
2023-10-04 16:04:32.793  INFO 64 --- [           main] o.a.c.c.C.[Tomcat].[localhost].[/]       : Initializing Spring embedded WebApplicationContext
2023-10-04 16:04:32.800  INFO 64 --- [           main] w.s.c.ServletWebServerApplicationContext : Root WebApplicationContext: initialization completed in 9192 ms
2023-10-04 16:04:34.767  INFO 64 --- [           main] o.s.s.concurrent.ThreadPoolTaskExecutor  : Initializing ExecutorService 'applicationTaskExecutor'
2023-10-04 16:04:36.954  INFO 64 --- [           main] o.s.b.w.embedded.tomcat.TomcatWebServer  : Tomcat started on port(s): 8085 (http) with context path ''
2023-10-04 16:04:37.063  INFO 64 --- [           main] example.Application                      : Started Application in 32.279 seconds (JVM running for 36.651)
Login: testuser
Password: test123

```
The warning messages in the log indicate some kind of race condition between defining and accssing the Vault `uri` by Spring, but the eventually logged sensitive data proves the success

if it logs an error:

```text
2023-07-06 01:42:24.987 ERROR 240 --- [           main] o.s.boot.SpringApplication               : Application run failed

org.springframework.beans.factory.BeanCreationException: Error creating bean with name 'vaultConfiguration': Injection of autowired dependencies failed; nested exception is java.lang.IllegalArgumentException: Could not resolve placeholder 'login' in value "${login}"
```
it means it was misconfigured

if you see the error
```text
Web server failed to start. Port 8085 was already in use.
```
connect to the container interactively and terminate the already running Java process

* you can update the sensitive informaion in the vault and see it being picked by the new instance of the Java app
### TODO 

* Refactor `Dockerfile` to prevent repeating the
```text
Step 4/13 : ADD https://releases.hashicorp.com/vault/${VAULT_VERSION}/vault_${VAULT_VERSION}_linux_amd64.zip .
Downloading  81.38MB/81.38MB

 ---> Using cache
 ---> 5dbbac5d6289
Step 5/13 : RUN unzip vault_${VAULT_VERSION}_linux_amd64.zip -d /usr/local/bin && rm vault_${VAULT_VERSION}_linux_amd64.zip
 ---> Using cache
```

- it does appear to be using cache but after the download
### See Also

  * __Hashicorp Vault__
     + [Getting Started with Vault](https://app.pluralsight.com/lti-integration/redirect/3134d6b5-8d8f-48fe-9251-b3ec443fa9f5)(qwicklab)

     + [Managing Vault Tokens](https://app.pluralsight.com/lti-integration/redirect/adb24492-f4c6-4417-baab-50e212f1522e)(qwicklab)

     + [Interacting with Vault Policies](https://app.pluralsight.com/lti-integration/redirect/382af62b-8ffd-45ce-954c-3f27ae116189)(qwicklab)

     + [Authentication, Authorization, and Identity with Vault](https://app.pluralsight.com/lti-integration/redirect/4152971b-ee4e-4f56-bc6d-5055197e2b4a)(qwicklab)

     + [Creating Dynamic Secrets for Google Cloud with Vault](https://app.pluralsight.com/lti-integration/redirect/eb4f5638-7458-4b95-95ef-23067291c0af)(qwicklab) `GSP1007`

  * [Azure Vault](https://learn.microsoft.com/en-us/azure/key-vault/general/basic-concepts)

  * [Azure Vault Keys,Secrets and Certificates](https://learn.microsoft.com/en-us/azure/key-vault/general/about-keys-secrets-certificates)

  * __Pluralsight__
     + [Configuring and Managing Microsoft Azure Key Vault](https://app.pluralsight.com/library/courses/microsoft-azure-key-vault-configuring-managing)
     + [Implementing and Managing HashiCorp Vault](https://app.pluralsight.com/paths/skill/implementing-and-managing-hashicorp-vault) (path)
 
  * [Azure Key Vault](https://www.baeldung.com/spring-cloud-azure-key-vault)
  * [An Intro to Vault](https://www.baeldung.com/vault)
  * [HashiCorp Spring Vault](https://www.baeldung.com/spring-vault)
  * [Load a secret from Azure Key Vault in a Spring Boot application](https://learn.microsoft.com/en-us/azure/developer/java/spring-framework/configure-spring-boot-starter-java-app-with-azure-key-vault)
  * likely an useful [project](https://github.com/markramach/vault-spring-boot-starter). NOTE, its parent class is the `org.springframework.cloud.spring-cloud-config`, not a usual `org.springframework.spring-boot-starter-parent`
  * [vendor example](https://github.com/hashicorp/hello-vault-spring/blob/main/sample-app/pom.xml) also uses the same parent pom
  * [skeleton springboot project with vault](https://github.com/codeforgeyt/vault-demo-mvn) - minimal configuration 
  * https://developer.hashicorp.com/vault/docs/commands
  * https://developer.hashicorp.com/vault/tutorials/tokens/tokens
  
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
