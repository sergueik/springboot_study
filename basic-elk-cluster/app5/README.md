### Info
this  directory contains a dummy plain Java SOAP server applicstion for APM evaluation.

### Note

for Java 11, added [stackoverflow](https://stackoverflow.com/questions/51892528/migration-jaxws-application-from-java-8-to-java-11) recommended fix. Note: cannot use late versions, e.g. `3.0.0`, there will be original errors, seen first without no `jws-rt` dependency in the `pom.xml`:

```text
ERROR] Failed to execute goal org.apache.maven.plugins:maven-compiler-plugin:3.6.1:compile (default-compile) on project soap-service: Compilation failure: Compilation failure:
[ERROR] /home/sergueik/src/springboot_study/basic-elk-cluster/app5/src/main/java/example/Application.java:[5,17] package javax.jws does not exist
[ERROR] /home/sergueik/src/springboot_study/basic-elk-cluster/app5/src/main/java/example/Application.java:[6,17] package javax.jws does not exist
[ERROR] /home/sergueik/src/springboot_study/basic-elk-cluster/app5/src/main/java/example/Application.java:[7,22] package javax.jws.soap does not exist
[ERROR] /home/sergueik/src/springboot_study/basic-elk-cluster/app5/src/main/java/example/Application.java:[8,20] package javax.xml.ws does not exist
[ERROR] /home/sergueik/src/springboot_study/basic-elk-cluster/app5/src/main/java/example/Application.java:[13,2] cannot find symbol
[ERROR]   symbol: class WebService
[ERROR] /home/sergueik/src/springboot_study/basic-elk-cluster/app5/src/main/java/example/Application.java:[14,2] cannot find symbol
[ERROR]   symbol: class SOAPBinding
[ERROR] /home/sergueik/src/springboot_study/basic-elk-cluster/app5/src/main/java/example/Application.java:[14,33] package SOAPBinding does not exist
[ERROR] /home/sergueik/src/springboot_study/basic-elk-cluster/app5/src/main/java/example/Application.java:[18,10] cannot find symbol
[ERROR]   symbol:   class WebMethod
[ERROR]   location: class example.Application
[ERROR] /home/sergueik/src/springboot_study/basic-elk-cluster/app5/src/main/java/example/Application.java:[36,10] cannot find symbol
[ERROR]   symbol:   class WebMethod
[ERROR]   location: class example.Application
[ERROR] /home/sergueik/src/springboot_study/basic-elk-cluster/app5/src/main/java/example/Application.java:[47,17] cannot find symbol
[ERROR]   symbol:   variable Endpoint
[ERROR]   location: class example.Application
[ERROR] -> [Help 1]

```

### See Also

   *  https://stackoverflow.com/questions/51892528/migration-jaxws-application-from-java-8-to-java-11
