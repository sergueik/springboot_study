### Info

This directory containes example from [Obfuscate Spring Boot Applications with Proguard Maven Plugin example](https://medium.com/@ufuk.guler/obfuscate-spring-boot-applications-with-proguard-maven-plugin-1f34bb871776) modified to compile (the lombok was not working)


### Usage

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
IMAGE=proguard-spring-boot-example
docker build -t $IMAGE -f Dockerfile .
IMAGE=proguard-spring-boot-example
CONTAINER=proguard-spring-boot-example
docker run -p 8080:8080 --name $CONTAINER -d $IMAGE
```
```sh
until [ "$(docker inspect -f '{{.State.Status}}' $CONTAINER)" == "running" ]; do sleep 1; done;
```
> NOTE: risky, the app in the container may fail to start, 

```sh
docker logs $CONTAINER
```
logs (taken from another project) will explain, why
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
### See Also
   * https://medium.com/@ufuk.guler/obfuscate-spring-boot-applications-with-proguard-maven-plugin-1f34bb871776
  * https://stackoverflow.com/questions/52875698/how-to-proguard-with-spring-boot-gradle-plugin
  * https://www.guardsquare.com/manual/configuration/examples
  * https://stackoverflow.com/questions/12114096/how-do-i-use-proguard
  * https://stackoverflow.com/questions/27714914/android-how-to-check-proguard-obfuscation-has-worked
  * https://medium.com/@jonfinerty/beginner-to-proguard-b3327ff3a831
  * https://habr.com/ru/articles/415499 (in Russian)

---

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
