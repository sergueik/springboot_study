#### Info

code at `bb926d3bdeefc8de38987e8ac97ceefbaf23d6ac` from
[Spring Vault](https://www.baeldung.com/spring-vault) and a basic __HashiCorp Vault on Alpine__ `Dockerfile` based on [project](https://github.com/dweomer/dockerfiles-vault). Alternatively one may try the vendor build vault image from [hashicorp vault docker container](https://hub.docker.com/_/vault/tags?page=1)

### Usage
* build package on host (NOTE: skip the tests)
```sh
mvn -Dmaven.test.skip=true package
```
* build image
```sh
IMAGE=basic-vault
docker build -t $IMAGE -f Dockerfile .
```
* run, override entry point
```sh
docker run --entrypoint "" -it $IMAGE sh
```
```sh
vault -version
``
```text
Vault v1.12.2 (415e1fe3118eebd5df6cb60d13defdc01aa17b03), built 2022-11-23T12:53
:46Z
```
* start dev server 
```sh
vault server -dev
```
collect the token information to continue
```text
Root Token: hvs.LhxlbD0lD1QWV33DFfsSlauA
```

* do exercises from a separate console
```sh
IMAGE=basic-vault
CONTAINER_ID=$(docker container ls | grep $IMAGE |awk '{print $1}')
docker exec -it $CONTAINER_ID sh
```
```sh
vault kv put secret/hello foo=world
```
```text
== Secret Path ==
secret/data/hello

======= Metadata =======
Key                Value
---                -----
created_time       2023-07-03T21:03:15.379884493Z
custom_metadata    <nil>
deletion_time      n/a
destroyed          false
version            1
```

```sh
vault kv get -format=json secret/hello |  jq -r '.'
```

```json
{
  "request_id": "1ea52b23-98f0-4815-ac55-82e6a6111458",
  "lease_id": "",
  "lease_duration": 0,
  "renewable": false,
  "data": {
    "data": {
      "foo": "world"
    },
    "metadata": {
      "created_time": "2023-07-03T21:03:15.379884493Z",
      "custom_metadata": null,
      "deletion_time": "",
      "destroyed": false,
      "version": 1
    }
  },
  "warnings": null
}
```
TODO: add debugging logs to application 

### Usage
* pull vault image
```sh
docker pull vault:1.12.0
```
* run vault in foreground
```sh
docker run -it -p 8200:8200 vault:1.12.0
```
 * uppdate the code and properies with hostname where vault it run,
```sh
docker-mahine ip
```
or `localhst` if run on Linux host

 * attept to run tests
```sh
mvn test
```
this fails, apparently Spring is attempting to run `vault` command locally:
```text
starting vault
unable to start vault in new processjava.io.IOException: Cannot run program "vau
lt":
CreateProcess error=2, 
The system cannot find the file specified
```
```text
19:30:49.334 [main] WARN org.springframework.context.support.GenericApplicationContext - 
Exception encountered during context initialization - cancelling refresh attempt: org.springframework.beans.factory.UnsatisfiedDependencyException: 
Error creating bean with name 'credentialsService': Unsatisfied dependency expressed through constructor parameter 0; 
nested exception is org.springframework.beans.factory.BeanCreationException: 
Error creating bean with name 'vaultTemplate' defined in example.VaultTestConfiguration: 
Bean instantiation via factory method failed; nested exception is org.springframework.beans.BeanInstantiationException: Failed to instantiate [org.springframework.vault.core.VaultTemplate]: 
Factory method 'vaultTemplate' threw exception; nested exception is org.springframework.beans.factory.BeanCreationException: 
Error creating bean with name 'vaultInitializer' defined in example.VaultTestConfiguration: 
Bean instantiation via factory method failed; nested exception is org.springframework.beans.BeanInstantiationException: 
Failed to instantiate [example.VaultInitializer]: Factory method 'vaultInitializer' threw exception; 
nested exception is java.lang.NullPointerException
19:30:49.339 [main] DEBUG org.springframework.test.context.support.AbstractGenericContextLoader - Loading ApplicationContext for merged context configuration [[WebMergedContextConfiguration@15888343 testClass = VaultIntegrationManualTest, locations = '{}', classes = '{class example.CredentialsService, class example.VaultTestConfiguration}', contextInitializerClasses = '[]', activeProfiles = '{}',
propertySourceLocations = '{}', propertySourceProperties = '{org.springframework.boot.test.context.SpringBootTestContextBootstrapper=true, server.port=0}', contextCustomizers = set[[ImportsContextCustomizer@33ecda92 key = [@org.springframework.test.context.ContextConfiguration(inheritInitializers=true, loader=class org.springframework.test.context.support.AnnotationConfigContextLoader, initializers=[], classes=[class example.VaultTestConfiguration], name=, locations=[], value=[], inheritLocations=true), @org.springframework.vault.repository.configuration.EnableVaultRepositories(vaultTemplateRef=vaultTemplate, repositoryBaseClass=class org.springramework.data.repository.config.DefaultRepositoryBaseClass, excludeFilters=[], keyValueTemplateRef=vaultKeyValueTemplate, basePackageClasses=[], includeFilters=[], repositoryFactoryBeanClass=class org.springframework.vault.repository.support.VaultRepositoryFactoryBean, queryLookupStrategy=CREATE_IF_NOT_FOUND, namedQueriesLocation=, repositoryImplementationPostfix=Impl, considerNestedRepositories=false, basePackages=[], value=[]), @org.springframework.test.context.BootstrapWith(value=class org.springframework.boot.test.context.SpringBootTestContextBootstrapper), @org.apiguardian.api.API(consumers=[*], since=5.0, status=STABLE), @org.junit.runner.RunWith(value=class org.springframework.test.context.junit4.SpringJUnit4ClassRunner),
@org.springframework.test.annotation.DirtiesContext(hierarchyMode=EXHAUSTIVE, methodMode=AFTER_METHOD, classMode=AFTER_CLASS),
@org.springframework.boot.test.context.SpringBootTest(args=[], webEnvironment=RANDOM_PORT, value=[], properties=[], classes=[class example.CredentialsService]),
 @org.junit.jupiter.api.extension.ExtendWith(value=[class org.springframework.test.context.junit.jupiter.SpringExtension]), @org.springframework.data.keyvalue.repository.config.QueryCreatorType(repositoryQueryType=class org.springframework.vault.repository.query.VaultPartTreeQuery, value=class org.springframework.vault.repository.query.VaultQueryCreator), @org.springframework.context.annotation.Import(value=[class org.springframework.vault.repository.configuration.VaultRepositoriesRegistrar])]], org.springframework.boot.test.context.filter.ExcludeFilterContextCustomizer@4df50bcc, org.springframework.boot.test.json.DuplicateJsonObjectContextCustomizerFactory$DuplicateJsonObjectContextCustomizer@54c562f7, org.springframework.boot.test.mock.mockito.MockitoContextCustomizer@0, org.springframework.boot.test.web.client.TestRestTemplateContextCustomizer@2b6faea6, org.springframework.boot.test.autoconfigure.properties.PropertyMappingContextCustomizer@0,org.springframework.boot.test.autoconfigure.web.servlet.WebDriverContextCustomizerFactory$Customizer@79b06cab, org.springframework.boot.test.context.SpringBootTestArgs@1], resourceBasePath = 'src/main/webapp', contextLoader = 'org.springframework.test.context.support.AnnotationConfigContextLoader', parent = [null]]].
Caused by: org.springframework.beans.BeanInstantiationException: Failed to instantiate [org.springframework.vault.core.VaultTemplate]: 
Factory method 'vaultTemplate' threw exception; nested exception is org.springframework.beans.factory.BeanCreationException: 
Error creating bean with name 'vaultInitializer' defined inexample.VaultTestConfiguration: Bean instantiation via factory method failed; nested exception is org.springframework.beans.BeanInstantiationException: Failed to instantiate [example.VaultInitializer]: Factory method 'vaultInitializer' threw exception; nested exception is java.lang.NullPointerException
        at org.springframework.beans.factory.support.SimpleInstantiationStrategy.instantiate(SimpleInstantiationStrategy.java:185)
        at org.springframework.beans.factory.support.ConstructorResolver.instantiate(ConstructorResolver.java:650)
        ... 77 common frames omitted
```
NOTE: the `starting vault` is logged regardless the Docker container is running or not


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

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
