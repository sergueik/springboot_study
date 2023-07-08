#### Info

this directory contains replica of the skeleton
springboot vault-backed spring cloud configuration [example repository](https://github.com/codeforgeyt/vault-demo-mvn)
connected to Vault [running in Docker container](https://github.com/dweomer/dockerfiles-vault/blob/master/Dockerfile).

the steps to configure the Vault and `bootstrap.properties` are covered in [youtube video](https://youtu.be/MaTDiKp_IrA)

### Usage
* build package on host (NOTE: skip the tests)
```sh
mvn -Dmaven.test.skip=true package
```
* build image
```sh
export IMAGE=basic-vault
docker build --build-arg "GID=$(id -g)" --build-arg "UID=$(id -u)" -t $IMAGE -f Dockerfile .
```
* NOTE: on Windows the values returned by `id` will be big numbers like `197609`
* run, override the entry point, and exposing the default port
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
and remain running

* NOTE: simple PLAIN strings like 'example' appear to not always work - only observer through Java aplication failure

* nake sure use the `spring.cloud.vault.token` value from `src/main/resources/bootstrap.properties`
this will allow connecting to the container through the ip address of the dev host:

![Vault UI](https://github.com/sergueik/springboot_study/blob/master/basic-spring-vault/screenshots/capture-login.png)


alternatively can drop the `dev-root-token-id` option and

collect the randomly generated token information to continue
```text
Root Token: hvs.LhxlbD0lD1QWV33DFfsSlauA
```
update `src/main/resources/bootstrap.properties`, repackage, copy
```sh
export ID=$(docker container ls -a| grep $IMAGE| awk '{print $1}')

docker cp target/example.basic-vault.jar $ID:/work/app.jar
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
NOTE:  combine both inputs in single command (may not be necessary)
```sh
vault kv put secret/application login=testuser password=test123
```
(the `secret` secret engine and the `application` path are the default ones Java uses)

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
![Secret](https://github.com/sergueik/springboot_study/blob/master/basic-spring-vault/screenshots/capture-secret.png)

* run the Java app

```sh
java -jar app.jar
```
this will log Spring logo and possibly few ignorable warnings, then
```text
2023-07-06 01:45:32.679  INFO 260 --- [           main] c.c.v.VaultDemoMvnApplication            : Started VaultDemoMvnApplication in 9.565 seconds (JVM running for 10.926)
Login: testuser
Password: test123

```
if it logs an error:

```text
2023-07-06 01:42:24.987 ERROR 240 --- [           main] o.s.boot.SpringApplication               : Application run failed

org.springframework.beans.factory.BeanCreationException: Error creating bean with name 'vaultConfiguration': Injection of autowired dependencies failed; nested exception is java.lang.IllegalArgumentException: Could not resolve placeholder 'login' in value "${login}"

```
it means it was misconfigured

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
