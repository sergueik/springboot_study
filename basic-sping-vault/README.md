#### Info

code at `bb926d3bdeefc8de38987e8ac97ceefbaf23d6ac` from
[Spring Vault](https://www.baeldung.com/spring-vault)
with vault from [hashicorp vault docker container](https://hub.docker.com/_/vault/tags?page=1)

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
19:30:49.334 [main] WARN org.springframework.context.support.GenericApplicationC
ontext - Exception encountered during context initialization - cancelling refres
h attempt: org.springframework.beans.factory.UnsatisfiedDependencyException: Err
or creating bean with name 'credentialsService': Unsatisfied dependency expresse
d through constructor parameter 0; nested exception is org.springframework.beans
.factory.BeanCreationException: Error creating bean with name 'vaultTemplate' de
fined in example.VaultTestConfiguration: Bean instantiation via factory method f
ailed; nested exception is org.springframework.beans.BeanInstantiationException:
 Failed to instantiate [org.springframework.vault.core.VaultTemplate]: Factory m
ethod 'vaultTemplate' threw exception; nested exception is org.springframework.b
eans.factory.BeanCreationException: Error creating bean with name 'vaultInitiali
zer' defined in example.VaultTestConfiguration: Bean instantiation via factory m
ethod failed; nested exception is org.springframework.beans.BeanInstantiationExc
eption: Failed to instantiate [example.VaultInitializer]: Factory method 'vaultI
nitializer' threw exception; nested exception is java.lang.NullPointerException
19:30:49.339 [main] DEBUG org.springframework.test.context.support.AbstractGener
icContextLoader - Loading ApplicationContext for merged context configuration [[
WebMergedContextConfiguration@15888343 testClass = VaultIntegrationManualTest, l
ocations = '{}', classes = '{class example.CredentialsService, class example.Vau
ltTestConfiguration}', contextInitializerClasses = '[]', activeProfiles = '{}',
propertySourceLocations = '{}', propertySourceProperties = '{org.springframework
.boot.test.context.SpringBootTestContextBootstrapper=true, server.port=0}', cont
extCustomizers = set[[ImportsContextCustomizer@33ecda92 key = [@org.springframew
ork.test.context.ContextConfiguration(inheritInitializers=true, loader=class org
.springframework.test.context.support.AnnotationConfigContextLoader, initializer
s=[], classes=[class example.VaultTestConfiguration], name=, locations=[], value
=[], inheritLocations=true), @org.springframework.vault.repository.configuration
.EnableVaultRepositories(vaultTemplateRef=vaultTemplate, repositoryBaseClass=cla
ss org.springframework.data.repository.config.DefaultRepositoryBaseClass, exclud
eFilters=[], keyValueTemplateRef=vaultKeyValueTemplate, basePackageClasses=[], i
ncludeFilters=[], repositoryFactoryBeanClass=class org.springframework.vault.rep
ository.support.VaultRepositoryFactoryBean, queryLookupStrategy=CREATE_IF_NOT_FO
UND, namedQueriesLocation=, repositoryImplementationPostfix=Impl, considerNested
Repositories=false, basePackages=[], value=[]), @org.springframework.test.contex
t.BootstrapWith(value=class org.springframework.boot.test.context.SpringBootTest
ContextBootstrapper), @org.apiguardian.api.API(consumers=[*], since=5.0, status=
STABLE), @org.junit.runner.RunWith(value=class org.springframework.test.context.
junit4.SpringJUnit4ClassRunner), @org.springframework.test.annotation.DirtiesCon
text(hierarchyMode=EXHAUSTIVE, methodMode=AFTER_METHOD, classMode=AFTER_CLASS),
@org.springframework.boot.test.context.SpringBootTest(args=[], webEnvironment=RA
NDOM_PORT, value=[], properties=[], classes=[class example.CredentialsService]),
 @org.junit.jupiter.api.extension.ExtendWith(value=[class org.springframework.te
st.context.junit.jupiter.SpringExtension]), @org.springframework.data.keyvalue.r
epository.config.QueryCreatorType(repositoryQueryType=class org.springframework.
vault.repository.query.VaultPartTreeQuery, value=class org.springframework.vault
.repository.query.VaultQueryCreator), @org.springframework.context.annotation.Im
port(value=[class org.springframework.vault.repository.configuration.VaultReposi
toriesRegistrar])]], org.springframework.boot.test.context.filter.ExcludeFilterC
ontextCustomizer@4df50bcc, org.springframework.boot.test.json.DuplicateJsonObjec
tContextCustomizerFactory$DuplicateJsonObjectContextCustomizer@54c562f7, org.spr
ingframework.boot.test.mock.mockito.MockitoContextCustomizer@0, org.springframew
ork.boot.test.web.client.TestRestTemplateContextCustomizer@2b6faea6, org.springf
ramework.boot.test.autoconfigure.properties.PropertyMappingContextCustomizer@0,
org.springframework.boot.test.autoconfigure.web.servlet.WebDriverContextCustomiz
erFactory$Customizer@79b06cab, org.springframework.boot.test.context.SpringBootT
estArgs@1], resourceBasePath = 'src/main/webapp', contextLoader = 'org.springfra
mework.test.context.support.AnnotationConfigContextLoader', parent = [null]]].
Caused by: org.springframework.beans.BeanInstantiationException: Failed to insta
ntiate [org.springframework.vault.core.VaultTemplate]: Factory method 'vaultTemp
late' threw exception; nested exception is org.springframework.beans.factory.Bea
nCreationException: Error creating bean with name 'vaultInitializer' defined in
example.VaultTestConfiguration: Bean instantiation via factory method failed; ne
sted exception is org.springframework.beans.BeanInstantiationException: Failed t
o instantiate [example.VaultInitializer]: Factory method 'vaultInitializer' thre
w exception; nested exception is java.lang.NullPointerException
        at org.springframework.beans.factory.support.SimpleInstantiationStrategy
.instantiate(SimpleInstantiationStrategy.java:185)
        at org.springframework.beans.factory.support.ConstructorResolver.instant
iate(ConstructorResolver.java:650)
        ... 77 common frames omitted
```
NOTE: the `starting vault` is logged regardless the Docker container is running or not


### See Also

  * https://www.baeldung.com/spring-cloud-azure-key-vault
  * https://learn.microsoft.com/en-us/azure/developer/java/spring-framework/configure-spring-boot-starter-java-app-with-azure-key-vault



