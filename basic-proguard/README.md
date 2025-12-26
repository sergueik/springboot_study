### Info

This directory containes example from [Obfuscate Spring Boot Applications with Proguard Maven Plugin example](https://medium.com/@ufuk.guler/obfuscate-spring-boot-applications-with-proguard-maven-plugin-1f34bb871776) modified to compile (the lombok was not working)


### Usage

To succeeded with [ProGuard]() obfuscation of an dependency-heavy, DI-intensive Spring Boot / WebFlux apps one typically needs to Treat ProGuard as a linker with deliberate rule curation not a text transformer and preserve Spring framework internals untouched
+ annotations
+ reflection targets
+ proxy interfaces
+ configuration properties

and obfuscate *business logic* packages only

Consider the ProGuard mapping file becomes a private ABI between the runtime abd observability:

+ Containers expose minimal symbolic surface
+ Runtime stack traces are unintelligible without the map: APM gets legiblt transactions, attacker gets debugging noise

Dependency-heavy microservices need symbol clarity more than raw logs.

APMs almost always store telemetry in its original (obfuscated / cryptic) form and resolve to real symbols at query or render time. This is same architectural pattern as the Microsoft Symbol Server + [PDB](https://en.wikipedia.org/wiki/Program_database): during event ingestion, APM backend stores exactly what the agent sends: class and method names obfuscated,
service name and version known through [trace context](https://www.w3.org/TR/trace-context/) `tracestate` which is specifically desiged for vendor-specific metadata. The symbol expansion happens late, not during ingestion, but during query/aggregation/render. Some derived metrics (aggregations, rollups) may store normalized names.

#### Build the App Jar

```sh
mvn clean package
```

open the application jar in [Java Class Viewer](https://www.codeproject.com/Articles/35915/Java-Class-Viewer)

![Original Jar](https://github.com/sergueik/springboot_study/blob/master/basic-proguard/screenshots/original_jar.png)

#### Proguard The Jar

```sh
mvn -Pproguard clean package
```

the build log will contain (truncated):
```text
[INFO] proguard jar: [C:\Users\kouzm\.m2\repository\com\guardsquare\proguard-base\7.2.0-beta4\proguard-base-7.2.0-beta4.jar, C:\Users\kouzm\.m2\repository\com\guardsquare\proguard-core\8.0.3\proguard-core-8.0.3.jar]
 [proguard] ProGuard, version 7.2.0-beta4
```
open the application jar in [Java Class Viewer](https://github.com/amosshi/freeinternals/tree/master/JavaClassViewer)

![ProGuard Jar](https://github.com/sergueik/springboot_study/blob/master/basic-proguard/screenshots/proguard_jar.png)

the `proguard_map.txt` contains the deobfuscation mapping:
```cmd
findstr -i vjbt target\proguard_map.txt
```
```txt
com.bookportal.api.configs.EnvironmentVariables -> com.bookportal.api.proguard.vjBtQDEgRu:
    34:34:java.lang.String facebookIdAlreadyInUse() -> vjBtQDEgRu
    com.bookportal.api.service.BookService bookService -> vjBtQDEgRu
    48:50:reactor.core.publisher.Mono refuseComment(java.lang.String) -> vjBtQDEgRu
    com.bookportal.api.service.BookService bookService -> vjBtQDEgRu
    com.bookportal.api.service.BookService bookService -> vjBtQDEgRu
    com.bookportal.api.service.BookService bookService -> vjBtQDEgRu
    51:59:reactor.core.publisher.Mono updatePassword(reactor.core.publisher.Mono) -> vjBtQDEgRu
    22:23:reactor.core.publisher.Mono getPublishersByPagination(int,int) -> vjBtQDEgRu
    59:61:void lambda$save$1(java.lang.Throwable) -> vjBtQDEgRu
    com.bookportal.api.service.BookService bookService -> vjBtQDEgRu
    97:98:reactor.core.publisher.Mono initComment(java.lang.String,java.lang.String) -> vjBtQDEgRu
    55:58:com.bookportal.api.entity.EmailConfirm initIfEmpty(com.bookportal.api.entity.User) -> vjBtQDEgRu

```
#### Package the Conatainer 
```sh
docker pull mvertes/alpine-mongo
docker pull eclipse-temurin:11-jre-alpine
docker pull eclipse-temurin:11-jdk-alpine
```
```sh
docker-compose up -build
```
if the error 

```trxt
#0 84.07 [ERROR] Failed to execute goal com.github.wvengen:proguard-maven-plugin:2.5.3:proguard (default) on project api: Obfuscation failed (result=1) -> [Help 1]
#0 84.07 [ERROR]
#0 84.07 [ERROR] To see the full stack trace of the errors, re-run Maven with the -e switch.
#0 84.07 [ERROR] Re-run Maven using the -X switch to enable full debug logging.
#0 84.07 [ERROR]
#0 84.07 [ERROR] For more information about the errors and possible solutions, please read the following articles:
#0 84.07 [ERROR] [Help 1] http://cwiki.apache.org/confluence/display/MAVEN/MojoExecutionException
------
failed to solve: failed to solve with frontend dockerfile.v0: failed to build LLB: executor failed running [/bin/sh -c mvn -T 2C -Pproguard -Dmaven.test.skip=true clean package]: runc did not terminate sucessfully
```
is oberved and the `docker-compose`’s "plain" progress ncurses mode is broken in your environment ( the `COMPOSE_PROGRESS=plain` has effect of 
* early exit
* no output
* no error
* build not even starting
)  can run Dockerile

```sh
IMAGE=proguard-spring-boot-example
docker build -t $IMAGE -f Dockerfile .
```
this will reveal the error
```text
 [proguard] ProGuard, version 7.2.0-beta4
 [proguard] Unexpected error
 [proguard] java.io.IOException: The input doesn't contain any classes. Did you specify the proper '-injars' options?
 [proguard]     at proguard.InputReader.execute(InputReader.java:147) ~[proguard-base-7.2.0-beta4.jar:7.2.0-beta4]
 [proguard]     at proguard.ProGuard.readInput(ProGuard.java:310) ~[proguard-base-7.2.0-beta4.jar:7.2.0-beta4]
 [proguard]     at proguard.ProGuard.execute(ProGuard.java:119) ~[proguard-base-7.2.0-beta4.jar:7.2.0-beta4]
 [proguard]     at proguard.ProGuard.main(ProGuard.java:675) [proguard-base-7.2.0-beta4.jar:7.2.0-beta4]
```
Add the *in between* command to `Dockerifle`:
```sh
RUN mvn -T 2C -Dmaven.test.skip=true clean package
```
before
```sh
RUN mvn -T 2C -Pproguard package
```
and rebuild
see the errors
```text
[ERROR] Failed to execute goal org.springframework.boot:spring-boot-maven-plugin:2.5.5:repackage (repackage) on project api: Execution repackage of goal org.springframework.boot:spring-boot-maven-plugin:2.5.5:repackage failed: Unable to find main class -> [Help 1]
```
which are caused by improperly copying the local workspace into container and then
```text
 [proguard] Unexpected error while performing partial evaluation:
 [proguard]   Class       = [com/bookportal/api/proguard/WHbrCOvZqP]
 [proguard]   Method      = [KAIFhQQxYo()Lcom/bookportal/api/proguard/PvFGhOKrpX;]
 [proguard]   Exception   = [proguard.evaluation.IncompleteClassHierarchyException] (Can't find common super class of [org.springframework.boot.loader.archive.JarFileArchive] (with 1 known super classes: org.springframework.boot.loader.archive.JarFileArchive) and [org.springframework.boot.loader.archive.ExplodedArchive] (with 1 known super classes: org.springframework.boot.loader.archive.ExplodedArchive))
```
after that error it resolved continue as planned
```sh
IMAGE=proguard-spring-boot-example
CONTAINER=proguard-spring-boot-example
docker run -p 8080:8080 --name $CONTAINER -d $IMAGE
```
```sh
export CONTAINER=proguard-spring-boot-example;
until [ "$(docker inspect -f '{{.State.Status}}' $CONTAINER)" == "running" ]; do sleep 1; done; echo 'Done';
```
```text
Template parsing error: template: :1:8: executing "" at <.State.Status>: map has no entry for key "State"
Template parsing error: template: :1:8: executing "" at <.State.Status>: map has no entry for key "State"
Done
```
> NOTE: risky, the app in the container may fail to start, 

```sh
docker logs $CONTAINER
```
```text
Error: Invalid or corrupt jarfile app.jar
```
examine

```sh
docker rm $CONTAINER
docker run -it --entrypoint '' -p 8080:8080 --name $CONTAINER $IMAGE sh
```

The `mongo` dependency is unstable when run on Docker Toolbox hosted on Windows OS / Virtual Box:
```text
app-mongo   | {"t":{"$date":"2025-12-25T22:56:19.432+00:00"},"s":"F",  "c":"CONTROL",  "id":6384300, "ctx":"main","msg":"Writing fatal message","attr":{"message":"Invalid access at address: 0\n"}}
app-mongo   | {"t":{"$date":"2025-12-25T22:56:19.432+00:00"},"s":"F",  "c":"CONTROL",  "id":6384300, "ctx":"main","msg":"Writing fatal message","attr":{"message":"Dumping siginfo (si_code=128): 0b 00 00 00 00 00 00 00 80 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00\n"}}
app-mongo   | {"t":{"$date":"2025-12-25T22:56:19.432+00:00"},"s":"F",  "c":"CONTROL",  "id":6384300, "ctx":"main","msg":"Writing fatal message","attr":{"message":"Got signal: 11 (Segmentation fault).\n"}}
app-mongo   | {"t":{"$date":"2025-12-25T22:56:19.580+00:00"},"s":"I",  "c":"CONTROL",  "id":31380,   "ctx":"main","msg":"BACKTRACE","attr":{"bt":{"backtrace":[{"a":"5629CADC1D47","b":"5629C0ED9000","o":"9EE8D47","s":"_ZN5mongo15printStackTraceEv","C":"mongo::printStackTrace()","s+":"37"},{"a":"5629CADA0C45","b":"5629C0ED9000","o":"9EC7C45","s":"_ZN5mongo12_GLOBAL__N_115printErrorBlockEv","C":"mongo::(anonymous namespace)::printErrorBlock()","s+":"225"},{"a":"5629CADA0F30","b":"5629C0ED9000","o":"9EC7F30","s":"abruptQuitWithAddrSignal","s+":"150"},{"a":"7EFE804F9330","b":"7EFE804B4000","o":"45330"},{"a":"7EFE804DC9A2","b":"7EFE804B4000","o":"289A2","s":"abort","s+":"182"},{"a":"5629CB0B6D7B","b":"5629C0ED9000","o":"A1DDD7B","s":"_ZN10__cxxabiv111__terminateEPFvvE.cold","C":"__cxxabiv1::__terminate(void (*)()) [clone .cold]","s+":"D"},{"a":"5629CB0B6DF1","b":"5629C0ED9000","o":"A1DDDF1","s":"_ZSt9terminatev","C":"std::terminate()","s+":"12"},{"a":"5629C52EA1F8","b":"5629C0ED9000","o":"44111F8","s":"__cxa_throw","s+":"48"},{"a":"5629C53722D7","b":"5629C0ED9000","o":"44992D7","s":"__wrap___cxa_throw","s+":"57"},{"a":"5629CB1459A0","b":"5629C0ED9000","o":"A26C9A0","s":"_ZSt20__throw_system_errori","C":"std::__throw_system_error(int)","s+":"83"},{"a":"5629CB145BF3","b":"5629C0ED9000","o":"A26CBF3","s":"_ZNSt6thread15_M_start_threadESt10unique_ptrINS_6_StateESt14default_deleteIS1_EEPFvvE.cold","C":"std::thread::_M_start_thread(std::unique_ptr<std::thread::_State, std::default_delete<std::thread::_State> >, void (*)()) [clone .cold]","s+":"7"},{"a":"5629C662AD83","b":"5629C0ED9000","o":"5751D83","s":"_ZN5mongo27startSignalProcessingThreadENS_13LogFileStatusE","C":"mongo::startSignalProcessingThread(mongo::LogFileStatus)","s+":"1E3"},{"a":"5629C5409A36","b":"5629C0ED9000","o":"4530A36","s":"_ZN5mongo11mongod_mainEiPPc","C":"mongo::mongod_main(int, char**)","s+":"C6"},{"a":"5629C53F30E7","b":"5629C0ED9000","o":"451A0E7","s":"main","s+":"9"},{"a":"7EFE804DE1CA","b":"7EFE804B4000","o":"2A1CA"},{"a":"7EFE804DE28B","b":"7EFE804B4000","o":"2A28B","s":"__libc_start_main","s+":"8B"},{"a":"5629C53F2FC5","b":"5629C0ED9000","o":"4519FC5","s":"_start","s+":"25"}],"processInfo":{"mongodbVersion":"8.2.3","gitVersion":"36f41c9c30a2f13f834d033ba03c3463c891fb01","compiledModules":[],"uname":{"sysname":"Linux","release":"4.19.130-boot2docker","version":"#1 SMP Mon Jun 29 23:52:55 UTC 2020","machine":"x86_64"},"somap":[{"b":"5629C0ED9000","path":"/usr/bin/mongod","elfType":3,"buildId":"FD88BF8A90D336B4DEF4B7E1CA6695FA49D31A34"},{"b":"7EFE804B4000","path":"/lib/x86_64-linux-gnu/libc.so.6","elfType":3,"buildId":"274EEC488D230825A136FA9C4D85370FED7A0A5E"}]}}},"tags":[]}
app-mongo   | {"t":{"$date":"2025-12-25T22:56:19.580+00:00"},"s":"I",  "c":"CONTROL",  "id":31445,   "ctx":"main","msg":"Frame","attr":{"frame":{"a":"5629CADC1D47","b":"5629C0ED9000","o":"9EE8D47","s":"_ZN5mongo15printStackTraceEv","C":"mongo::printStackTrace()","s+":"37"}}}
app-mongo   | {"t":{"$date":"2025-12-25T22:56:19.581+00:00"},"s":"I",  "c":"CONTROL",  "id":31445,   "ctx":"main","msg":"Frame","attr":{"frame":{"a":"5629CADA0C45","b":"5629C0ED9000","o":"9EC7C45","s":"_ZN5mongo12_GLOBAL__N_115printErrorBlockEv","C":"mongo::(anonymous namespace)::printErrorBlock()","s+":"225"}}}
app-mongo   | {"t":{"$date":"2025-12-25T22:56:19.581+00:00"},"s":"I",  "c":"CONTROL",  "id":31445,   "ctx":"main","msg":"Frame","attr":{"frame":{"a":"5629CADA0F30","b":"5629C0ED9000","o":"9EC7F30","s":"abruptQuitWithAddrSignal","s+":"150"}}}
app-mongo   | {"t":{"$date":"2025-12-25T22:56:19.581+00:00"},"s":"I",  "c":"CONTROL",  "id":31445,   "ctx":"main","msg":"Frame","attr":{"frame":{"a":"7EFE804F9330","b":"7EFE804B4000","o":"45330"}}}
app-mongo   | {"t":{"$date":"2025-12-25T22:56:19.582+00:00"},"s":"I",  "c":"CONTROL",  "id":31445,   "ctx":"main","msg":"Frame","attr":{"frame":{"a":"7EFE804DC9A2","b":"7EFE804B4000","o":"289A2","s":"abort","s+":"182"}}}
app-mongo   | {"t":{"$date":"2025-12-25T22:56:19.582+00:00"},"s":"I",  "c":"CONTROL",  "id":31445,   "ctx":"main","msg":"Frame","attr":{"frame":{"a":"5629CB0B6D7B","b":"5629C0ED9000","o":"A1DDD7B","s":"_ZN10__cxxabiv111__terminateEPFvvE.cold","C":"__cxxabiv1::__terminate(void (*)()) [clone .cold]","s+":"D"}}}
app-mongo   | {"t":{"$date":"2025-12-25T22:56:19.582+00:00"},"s":"I",  "c":"CONTROL",  "id":31445,   "ctx":"main","msg":"Frame","attr":{"frame":{"a":"5629CB0B6DF1","b":"5629C0ED9000","o":"A1DDDF1","s":"_ZSt9terminatev","C":"std::terminate()","s+":"12"}}}
app-mongo   | {"t":{"$date":"2025-12-25T22:56:19.582+00:00"},"s":"I",  "c":"CONTROL",  "id":31445,   "ctx":"main","msg":"Frame","attr":{"frame":{"a":"5629C52EA1F8","b":"5629C0ED9000","o":"44111F8","s":"__cxa_throw","s+":"48"}}}
app-mongo   | {"t":{"$date":"2025-12-25T22:56:19.582+00:00"},"s":"I",  "c":"CONTROL",  "id":31445,   "ctx":"main","msg":"Frame","attr":{"frame":{"a":"5629C53722D7","b":"5629C0ED9000","o":"44992D7","s":"__wrap___cxa_throw","s+":"57"}}}
app-mongo   | {"t":{"$date":"2025-12-25T22:56:19.583+00:00"},"s":"I",  "c":"CONTROL",  "id":31445,   "ctx":"main","msg":"Frame","attr":{"frame":{"a":"5629CB1459A0","b":"5629C0ED9000","o":"A26C9A0","s":"_ZSt20__throw_system_errori","C":"std::__throw_system_error(int)","s+":"83"}}}
app-mongo   | {"t":{"$date":"2025-12-25T22:56:19.583+00:00"},"s":"I",  "c":"CONTROL",  "id":31445,   "ctx":"main","msg":"Frame","attr":{"frame":{"a":"5629CB145BF3","b":"5629C0ED9000","o":"A26CBF3","s":"_ZNSt6thread15_M_start_threadESt10unique_ptrINS_6_StateESt14default_deleteIS1_EEPFvvE.cold","C":"std::thread::_M_start_thread(std::unique_ptr<std::thread::_State, std::default_delete<std::thread::_State> >, void (*)()) [clone .cold]","s+":"7"}}}
app-mongo   | {"t":{"$date":"2025-12-25T22:56:19.583+00:00"},"s":"I",  "c":"CONTROL",  "id":31445,   "ctx":"main","msg":"Frame","attr":{"frame":{"a":"5629C662AD83","b":"5629C0ED9000","o":"5751D83","s":"_ZN5mongo27startSignalProcessingThreadENS_13LogFileStatusE","C":"mongo::startSignalProcessingThread(mongo::LogFileStatus)","s+":"1E3"}}}
app-mongo   | {"t":{"$date":"2025-12-25T22:56:19.583+00:00"},"s":"I",  "c":"CONTROL",  "id":31445,   "ctx":"main","msg":"Frame","attr":{"frame":{"a":"5629C5409A36","b":"5629C0ED9000","o":"4530A36","s":"_ZN5mongo11mongod_mainEiPPc","C":"mongo::mongod_main(int, char**)","s+":"C6"}}}
app-mongo   | {"t":{"$date":"2025-12-25T22:56:19.583+00:00"},"s":"I",  "c":"CONTROL",  "id":31445,   "ctx":"main","msg":"Frame","attr":{"frame":{"a":"5629C53F30E7","b":"5629C0ED9000","o":"451A0E7","s":"main","s+":"9"}}}
app-mongo   | {"t":{"$date":"2025-12-25T22:56:19.583+00:00"},"s":"I",  "c":"CONTROL",  "id":31445,   "ctx":"main","msg":"Frame","attr":{"frame":{"a":"7EFE804DE1CA","b":"7EFE804B4000","o":"2A1CA"}}}
app-mongo   | {"t":{"$date":"2025-12-25T22:56:19.584+00:00"},"s":"I",  "c":"CONTROL",  "id":31445,   "ctx":"main","msg":"Frame","attr":{"frame":{"a":"7EFE804DE28B","b":"7EFE804B4000","o":"2A28B","s":"__libc_start_main","s+":"8B"}}}
app-mongo   | {"t":{"$date":"2025-12-25T22:56:19.584+00:00"},"s":"I",  "c":"CONTROL",  "id":31445,   "ctx":"main","msg":"Frame","attr":{"frame":{"a":"5629C53F2FC5","b":"5629C0ED9000","o":"4519FC5","s":"_start","s+":"25"}}}
app-mongo   | {"t":{"$date":"2025-12-25T22:56:19.584+00:00"},"s":"F",  "c":"CONTROL",  "id":6384300, "ctx":"main","msg":"Writing fatal message","attr":{"message":"\n"}}
app-mongo   | {"t":{"$date":"2025-12-25T22:56:19.584+00:00"},"s":"F",  "c":"CONTROL",  "id":6384300, "ctx":"main","msg":"Writing fatal message","attr":{"message":"\n"}}
app-mongo exited with code 139
```

### Errors From Obfuscation

When something is broken at app launch, investigate

A typical bad obfuscation-induced runtime exception logs (taken from another project) will explain, why
```text
 ____                                                         __        ____                                               ___
/\  _`\                                                      /\ \      /\  _`\                                            /\_ \
\ \ \L\ \ _ __    ___      __      __  __     __      _ __   \_\ \     \ \ \L\_\   __  _     __       ___ ___     _____   \//\ \       __
 \ \ ,__//\`'__\ / __`\  /'_ `\   /\ \/\ \  /'__`\   /\`'__\ /'_` \     \ \  _\L  /\ \/'\  /'__`\   /' __` __`\  /\ '__`\   \ \ \    /'__`\
  \ \ \/ \ \ \/ /\ \L\ \/\ \L\ \  \ \ \_\ \/\ \L\.\_ \ \ \/ /\ \L\ \     \ \ \L\ \\/>  </ /\ \L\.\_ /\ \/\ \/\ \ \ \ \L\ \   \_\ \_ /\  __/
   \ \_\  \ \_\ \ \____/\ \____ \  \ \____/\ \__/.\_\ \ \_\ \ \___,_\     \ \____/ /\_/\_\\ \__/.\_\\ \_\ \_\ \_\ \ \ ,__/   /\____\\ \____\
    \/_/   \/_/  \/___/  \/___L\ \  \/___/  \/__/\/_/  \/_/  \/__,_ /      \/___/  \//\/_/ \/__/\/_/ \/_/\/_/\/_/  \ \ \/    \/____/ \/____/
                           /\____/                                                                                \ \_\
                           \_/__/                                                                                  \/_/
2025-12-24 23:18:03,531 [INFO ] [main      ] [c.s.p.e.s.b.Application       ] - Starting Application v2.2.0 using Java 11.0.10 on eb4d7669f19b with PID 18 (/usr/src/proguard-spring-boot-example/target/spring.boot.jar started by root in /usr/src/proguard-spring-boot-example)
2025-12-24 23:18:03,539 [INFO ] [main      ] [c.s.p.e.s.b.Application       ] - No active profile set, falling back to default profiles: default
2025-12-24 23:18:04,305 [WARN ] [main      ] [o.s.b.w.s.c.AnnotationConfigServletWebServerApplicationContext] - Exception encountered during context initialization - cancelling refresh attempt: org.springframework.beans.factory.BeanDefinitionStoreException: Failed to parse configuration class [com.slm.proguard.example.spring.boot.Application]; nested exception is org.springframework.context.annotation.ConflictingBeanDefinitionException: Annotation-specified bean name 'a' for bean class [com.slm.proguard.example.spring.boot.a.a] conflicts with existing, non-compatible bean definition of same name and class [com.slm.proguard.example.spring.boot.c.a]
2025-12-24 23:18:04,404 [ERROR] [main      ] [o.s.b.SpringApplication       ] - Application run failed
org.springframework.beans.factory.BeanDefinitionStoreException: Failed to parse configuration class [com.slm.proguard.example.spring.boot.Application];
nested exception is org.springframework.context.annotation.ConflictingBeanDefinitionException:
Annotation-specified bean name 'a' for bean class [com.slm.proguard.example.spring.boot.a.a]
conflicts with existing, non-compatible bean definition of same name and class [com.slm.proguard.example.spring.boot.c.a]
	at org.springframework.context.annotation.ConfigurationClassParser.parse(ConfigurationClassParser.java:189)
	at org.springframework.context.annotation.ConfigurationClassPostProcessor.processConfigBeanDefinitions(ConfigurationClassPostProcessor.java:331)
	at org.springframework.context.annotation.ConfigurationClassPostProcessor.postProcessBeanDefinitionRegistry(ConfigurationClassPostProcessor.java:247)
	at org.springframework.context.support.PostProcessorRegistrationDelegate.invokeBeanDefinitionRegistryPostProcessors(PostProcessorRegistrationDelegate.java:311)
	at org.springframework.context.support.PostProcessorRegistrationDelegate.invokeBeanFactoryPostProcessors(PostProcessorRegistrationDelegate.java:112)
	at org.springframework.context.support.AbstractApplicationContext.invokeBeanFactoryPostProcessors(AbstractApplicationContext.java:746)
	at org.springframework.context.support.AbstractApplicationContext.refresh(AbstractApplicationContext.java:564)
	at org.springframework.boot.web.servlet.context.ServletWebServerApplicationContext.refresh(ServletWebServerApplicationContext.java:145)
	at org.springframework.boot.SpringApplication.refresh(SpringApplication.java:754)
	at org.springframework.boot.SpringApplication.refreshContext(SpringApplication.java:434)
	at org.springframework.boot.SpringApplication.run(SpringApplication.java:338)
	at org.springframework.boot.SpringApplication.run(SpringApplication.java:1343)
	at org.springframework.boot.SpringApplication.run(SpringApplication.java:1332)
	at com.slm.proguard.example.spring.boot.Application.main(Application.java:21)
	at java.base/jdk.internal.reflect.NativeMethodAccessorImpl.invoke0(Native Method)
	at java.base/jdk.internal.reflect.NativeMethodAccessorImpl.invoke(NativeMethodAccessorImpl.java:62)
	at java.base/jdk.internal.reflect.DelegatingMethodAccessorImpl.invoke(DelegatingMethodAccessorImpl.java:43)
	at java.base/java.lang.reflect.Method.invoke(Method.java:566)
	at org.springframework.boot.loader.MainMethodRunner.run(MainMethodRunner.java:49)
	at org.springframework.boot.loader.Launcher.launch(Launcher.java:108)
	at org.springframework.boot.loader.Launcher.launch(Launcher.java:58)
	at org.springframework.boot.loader.JarLauncher.main(JarLauncher.java:88)
Caused by: org.springframework.context.annotation.ConflictingBeanDefinitionException: Annotation-specified bean name 'a' for bean class [com.slm.proguard.example.spring.boot.a.a] conflicts with existing, non-compatible bean definition of same name and class [com.slm.proguard.example.spring.boot.c.a]
	at org.springframework.context.annotation.ClassPathBeanDefinitionScanner.checkCandidate(ClassPathBeanDefinitionScanner.java:349)
	at org.springframework.context.annotation.ClassPathBeanDefinitionScanner.doScan(ClassPathBeanDefinitionScanner.java:287)
	at org.springframework.context.annotation.ComponentScanAnnotationParser.parse(ComponentScanAnnotationParser.java:132)
	at org.springframework.context.annotation.ConfigurationClassParser.doProcessConfigurationClass(ConfigurationClassParser.java:296)
	at org.springframework.context.annotation.ConfigurationClassParser.processConfigurationClass(ConfigurationClassParser.java:250)
	at org.springframework.context.annotation.ConfigurationClassParser.parse(ConfigurationClassParser.java:207)
	at org.springframework.context.annotation.ConfigurationClassParser.parse(ConfigurationClassParser.java:175)
	... 21 common frames omitted
```

#### Copying Jar from the Image Without Running

* Create container without running it

```sh
IMAGE=proguard-spring-boot-example
CONTAINER=proguard-spring-boot-example
docker container rm -f $CONTAINER
docker create --name $CONTAINER  $IMAGE 
```
* copy the JAR file from the container to host
```sh
docker cp $CONTAINER:/app/app.jar target/app.jar
```
### Retrace

When an obfuscated program throws an exception, the resulting stack trace typically isn't very informative: 
the Class names and method names have been replaced by short meaningless strings. 
[ReTrace](https://www.guardsquare.com/manual/tools/retrace) is a companion tool for 
ProGuard and DexGuard that 'de-obfuscates' the stack traces.

One does not have to  build it [manually](https://www.guardsquare.com/manual/building) and simpkly download the release

```sh
VERSION=7.8.2
curl -skLo ~/Downloads/proguard.zip https://github.com/Guardsquare/proguard/releases/download/v${VERSION}/proguard-${VERSION}.zip
```
examine the contents of the release artifact:

```sh
unzip -l ~/Downloads/proguard.zip
```
extract the runtimes
```sh
VERSION=7.8.2
unzip -x  ~/Downloads/proguard.zip "proguard-${VERSION}/lib/*" "proguard-${VERSION}/bin/*"
```
```text
Archive:  /c/Users/kouzm/Downloads/proguard.zip
  inflating: proguard-7.8.2/lib/proguard.jar
  inflating: proguard-7.8.2/lib/proguardgui.jar
  inflating: proguard-7.8.2/lib/retrace.jar
  inflating: proguard-7.8.2/lib/proguard-ant.jar
  inflating: proguard-7.8.2/bin/proguardgui.bat
  inflating: proguard-7.8.2/bin/proguard.bat
  inflating: proguard-7.8.2/bin/retrace.sh
  inflating: proguard-7.8.2/bin/proguardgui.sh
  inflating: proguard-7.8.2/bin/retrace.bat
  inflating: proguard-7.8.2/bin/proguard.sh
```

optionally copy into tools and add to the PATH:
```sh
VERSION=7.8.2
cp -R proguard-7.8.2/ /c/tools/proguard
``` 


```text
put updating user  {"xTzLLlWzoD":1,"vjBtQDEgRu":"Alice","agHeRrpWNx":"alice@example.com"}
with param: {"name":""}
put set name: "null"
set  name:
2025-12-26 11:53:15.564 ERROR 25792 --- [nio-8080-exec-1] o.a.c.c.C.[.[.[/].[dispatcherServlet]    : Servlet.service() for servlet [dispatcherServlet] in context with path [] threw exception [Request processing failed; nested exception is java.lang.IllegalArgumentException: name is required] with root cause

java.lang.IllegalArgumentException: name is required
        at example.PzHKbKynPU.setName(User.java:45) ~[classes!/:0.5.0-SNAPSHOT]
        at example.ffyUbXipBD.ffyUbXipBD(Controller.java:73) ~[classes!/:0.5.0-SNAPSHOT]        
```

the clear test code exception is
```text
put updating user  {"id":1,"name":"Alice","email":"alice@example.com"}
with param: {"name":""}
put set name: "null"
set  name:
2025-12-26 11:56:43.558 ERROR 25192 --- [nio-8080-exec-1] o.a.c.c.C.[.[.[/].[dispatcherServlet]    : Servlet.service() for servlet [dispatcherServlet] in context with path [] threw exception [Request processing failed; nested exception is java.lang.IllegalArgumentException: name is required] with root cause

java.lang.IllegalArgumentException: name is required
        at example.model.User.setName(User.java:45) ~[classes!/:0.5.0-SNAPSHOT]
        at example.controller.Controller.put(Controller.java:73) ~[classes!/:0.5.0-SNAPSHOT]
        
```

save the cryptic exception fragment from application logs into a file (stacktrace.log). Recommended to save 
the *entire exception log*, including nested causes, for complete deobfuscation.

On Windows  host have to use `retrace.bat` since the shell version relies on unavailable utilities, and have Java path set (the will not be a good environment)

```cmd
c:\tools\proguard\bin\retrace.bat -verbose target\proguard_map.txt stacktrace.txt
```

![Retraced the exception](https://github.com/sergueik/springboot_study/blob/master/basic-proguard/screenshots/retraced.png)

### APM && Guard Square == 0

This appeaers like attempt to recreate the "symbol server" effect for Java microservices with Proguard obfuscation in the way.

Out of the box, __ELK__ does *NOT* use __ProGuard__ mapping files at all.

So in the minimal distributed tracing setup:

  * Two ProGuarded Spring Boot apps nodes
  * Elastic APM agent on both
  * Distributed HTTP call
  * Trace correlation via `trace.id`

In this setup,

  * The trace will be grouped correctly,
  * but caller/callee method / class names will appear verbatim as the agent sees them - as random-looking hashes, not clearText. 

This is because trace correlation

  + works out of the box
  + underlying `Trace ID` propagation is automatic
  + the Parent/child spans are grouped correctly based on trace `Trace Context` [spec](https://www.w3.org/TR/trace-context/#parent-id)

As of today, mainstream APM stacks (Elastic APM, OpenTelemetry-based pipelines, Jaeger, Zipkin) do not support using ProGuard/R8 mapping files to present cleartext Java class or method names in distributed traces at runtime.

This is not a configuration gap. It is an architectural limitation.

To make simultaneous obfuscation + cleartext tracing  work properly, an APM system would need all of the following:

  * A way to ingest mapping files
  * Version awareness (service.version)
  * Late binding of symbols (like PDBs)
  * Query-time or UI-time de‑obfuscation
  * Secure handling of IP-sensitive symbols

None of the current vendors implement this end‑to‑end.

Switching to Otel does not solve the obfuscation vs clarity problem.

Even __AppDynamics__ (and older APMs):

 heavy on-demand instrumentation allowed (At huge performance cost)

  * call graphs
  * argument values
  * object snapshots

but not

  * symbol-server-style remapping

all extra serices App offer relied on non-obfuscated bytecode.

One can define a hard trade-off triangle, where one can pick at most two:

  * Code obfuscation (IP protection)
  * Cleartext method-level tracing
  * Zero custom tooling

Today’s reality:

  * Vendors choose (2) + (3)
  * Security teams push for (1)
  * Nobody ships (1) + (2) + (3)

The common compromise is:

* Obfuscate code
* Do NOT rely on method names in tracing


Another important usage of Sybol Server was:

Certain Windows components shipped with reduced or restricted symbolic visibility due to security and IP concerns,
while full symbols remained available internally. For that purpose, Microsoft has long maintained:

* public symbol servers (limited symbols released here)
* internal symbol servers (full symbols available here)

#### Microsoft Symbol Server — historical context

__Microsoft Symbol Server__ was introduced in the late 1990s and became broadly usable in the early 2000s as part of the Windows NT debugging ecosystem. Its primary purpose was to allow post-mortem debugging of compiled binaries without shipping debug symbols directly with the software.

At a high level:

  * Windows binaries (EXE/DLL) are shipped without symbols
  * Debug symbols are stored separately in PDB files
  * Each PDB is uniquely identified by a GUID + age
  * Debuggers (Visual Studio, WinDbg) retrieve matching symbols on demand from a symbol server

This design enabled several critical capabilities:

  * Small production binaries
  * Protection of proprietary implementation details
  * Accurate stack traces and crash analysis
  * Debugging across OS, driver, and application boundaries

Symbol resolution is explicitly developer-initiated and happens after execution, typically during crash analysis or live debugging sessions.

### Symbol Server Ecosystem

In the early 2000s, the idea that a debugger could:

 * automatically extract version identifiers from a binary
 * construct a structured request (GUIDs, checksums, CPU architecture)
 * accurately query a remote HTTP endpoint
 * retrieve exactly the right symbol file
 * seamlessly merge it into a debugging session

was not obvious at all.

At the time, this required developers to learn and embrace few new concepts that :

  * debugging metadata could reside outside the binary
  * symbol identity was defined by content hashes and GUIDs, not filenames
  * remote services could be trusted to supply critical debugging data
  * URLs and request parameters could encode rich, machine-readable meaning

In hindsight, this looks trivial. For the early 2000s, it was a major conceptual shift.
The __Windows Symbol Server__ succeeded because Microsoft invested heavily in:

  * defining non-surprising, deterministic protocols
  * making symbol lookup boring and reliable
  * hiding complexity behind tooling (Visual Studio, WinDbg)
  * standardizing identifiers across compilers, linkers, and OS builds

By contrast, even modern Java APM systems:

* assume runtime symbols are already meaningful
* never had to solve late-binding symbol resolution
* evolved in a world where HTTP, JSON, and trace IDs were already “obvious”

Today, URL-based symbol resolution is taken for granted, but the Java observability ecosystem never had to re-learn that lesson 
— until obfuscation reintroduced the problem

What looks like a “missing feature” in APM is actually a missing historical pressure.

Native debugging required symbol servers.

Java observability did not, until IP protection became widespread in microservices.

As a result, the ecosystem optimized for:

  * runtime clarity
  * immediate semantics
  * low friction

and never built a post-hoc symbol resolution layer.

This makes your conclusion stronger:

The absence of mapping-aware distributed tracing is not an oversight; it is the consequence of a different evolutionary path

#### Could Microsoft technically offer “obfuscated but fully traceable” distributed tracing?

Yes. Unequivocally.

Microsoft already has:

* Compiler toolchains
* IL rewriting (for .NET)
* Build pipelines
* Symbol servers
* IDE integration
* Telemetry ingestion (Application Insights)
* Full version control across the stack

In other words:

Microsoft already owns every technical building block required to implement a Java-style ProGuard + mapping + APM experience — and more.

They could:

* Obfuscate IL
* Generate mapping/symbol artifacts
* Associate them with service.version
* Resolve symbols only inside Azure
* Never expose raw symbols to customers

This would be entirely consistent with their historical PDB model.

So why don’t they?

This is the important part — and it’s not technical.

The reasons are primarily strategic and economic:
* The audience didn’t demand it: Most Application Insights users:

  + run first-party code
  + value clarity over IP protection
  + do not obfuscate .NET services

Banks and governments are a smaller but demanding segment

 * Observability value decreases with obfuscation. The APM value proposition depends on:

+ readable method names
+ actionable traces

Obfuscation without perfect remapping would degrade UX

* Trust and liability. Offering: "Only visible to you through us"

means: Microsoft becomes custodian of:

 + sensitive IP metadata
 + symbol mappings

That raises:

 + compliance
 + contractual
 + legal questions

Especially for government customers.

* Azure already sells without it

Azure adoption is driven by:

 + platform breadth
 + compliance certifications
 + ecosystem lock-in

Obfuscation-aware tracing is not currently a top-tier differentiator.

* The hypothesis is valid — and valuable

This is a fair analytical observation, not a conspiracy theory:

Microsoft has previously shipped security-sensitive components with restricted symbolic visibility, while retaining full internal debugging capability. This demonstrates that the company is technically capable of offering developer-friendly observability over obfuscated code. The absence of such functionality in modern distributed tracing appears to be a strategic choice rather than a technical limitation.

This strengthens your earlier point rather than contradicting it:

The absence of symbol-aware APM is ecosystem-wide

Even vendors who could do it haven’t

Therefore:

Java APM vendors aren’t “behind”

They simply followed a different evolutionary path



### Note

* proguard versions should be pinned adequately 

```txt
[ERROR] Failed to execute goal com.github.wvengen:proguard-maven-plugin:2.5.3:proguard (default) on project api: Obfuscation failed ProGuard (proguard.ProGuard) not found in classpath -> [Help 1]
```
---

### See Also
   * https://medium.com/@ufuk.guler/obfuscate-spring-boot-applications-with-proguard-maven-plugin-1f34bb871776
  * https://stackoverflow.com/questions/52875698/how-to-proguard-with-spring-boot-gradle-plugin
  * https://www.guardsquare.com/manual/configuration/examples
  * https://stackoverflow.com/questions/12114096/how-do-i-use-proguard
  * https://stackoverflow.com/questions/27714914/android-how-to-check-proguard-obfuscation-has-worked
  * https://medium.com/@jonfinerty/beginner-to-proguard-b3327ff3a831
  * https://habr.com/ru/articles/415499 (in Russian)
  * https://huseyinabanozeng.blogspot.com/2018/07/obfuscating-spring-boot-projects-using.html
  * https://stackoverflow.com/questions/8814312/how-to-run-proguard-in-my-spring-mvc-application-no-jar-but-for-classes
  * https://www.sobyte.net/post/2021-11/use-proguard-maven-plugin-to-obfuscate-the-spring-boot-program/
  * [migrating COBOL to Java - why does one need ANTLR](https://habr.com/ru/companies/alfastrah/articles/980846/) (in Russian)
  * proguard maven plugins:
  + https://github.com/wvengen/proguard-maven-plugin
  + https://github.com/dingxin/proguard-maven-plugin

---

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
