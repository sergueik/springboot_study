### Info
__dockerized spring boot microservice__ [repositoory](https://github.com/ahsumon85/dockerized-spring-boot-microservice) with few minor fixes, switched to run  on __alpine jre 1.8__

### Usage

it turns out it is better to run indivdualcontainer is this order
```sh
docker-compose up --build -d eureka
docker-compose up --build -d gateway
docker-compose up --build
```

* the cluuster fails with
```text
Attaching to auth, eureka, gateway, item, sales
gateway  | 2023-02-26 19:56:20.309  INFO 1 --- [           main] s.c.a.AnnotationConfigApplicationContext : Refreshing org.springframework.context.annotation.AnnotationConfigApplicationContext@50134894: startup date [Sun Feb 26 19:56:20 GMT 2023]; root of context hierarchy
eureka   | 2023-02-26 19:56:23.105  INFO 1 --- [           main] trationDelegate$BeanPostProcessorChecker : Bean 'configurationPropertiesRebinderAutoConfiguration' of type [org.springframework.cloud.autoconfigure.ConfigurationPropertiesRebinderAutoConfiguration$$EnhancerBySpringCGLIB$$275ea081] is not eligible for getting processed by all BeanPostProcessors (for example: not eligible for auto-proxying)
gateway  | 2023-02-26 19:56:23.828  INFO 1 --- [           main] f.a.AutowiredAnnotationBeanPostProcessor : JSR-330 'javax.inject.Inject' annotation found and supported for autowiring
eureka   | 
eureka   |   .   ____          _            __ _ _
eureka   |  /\\ / ___'_ __ _ _(_)_ __  __ _ \ \ \ \
eureka   | ( ( )\___ | '_ | '_| | '_ \/ _` | \ \ \ \
eureka   |  \\/  ___)| |_)| | | | | || (_| |  ) ) ) )
eureka   |   '  |____| .__|_| |_|_| |_\__, | / / / /
eureka   |  =========|_|==============|___/=/_/_/_/
eureka   |  :: Spring Boot ::        (v2.3.4.RELEASE)
eureka   | 
eureka   | 2023-02-26 19:56:24.349  INFO 1 --- [           main] com.ahasan.eureka.EurekaServerRunner     : No active profile set, falling back to default profiles: default
gateway  | 2023-02-26 19:56:24.403  INFO 1 --- [           main] trationDelegate$BeanPostProcessorChecker : Bean 'configurationPropertiesRebinderAutoConfiguration' of type [org.springframework.cloud.autoconfigure.ConfigurationPropertiesRebinderAutoConfiguration$$EnhancerBySpringCGLIB$$f022ba3f] is not eligible for getting processed by all BeanPostProcessors (for example: not eligible for auto-proxying)
auth     | 2023-02-26 19:56:24.519  INFO 1 --- [           main] trationDelegate$BeanPostProcessorChecker : Bean 'org.springframework.cloud.autoconfigure.ConfigurationPropertiesRebinderAutoConfiguration' of type [org.springframework.cloud.autoconfigure.ConfigurationPropertiesRebinderAutoConfiguration$$EnhancerBySpringCGLIB$$8700ee17] is not eligible for getting processed by all BeanPostProcessors (for example: not eligible for auto-proxying)
auth     | 
auth     |   .   ____          _            __ _ _
auth     |  /\\ / ___'_ __ _ _(_)_ __  __ _ \ \ \ \
auth     | ( ( )\___ | '_ | '_| | '_ \/ _` | \ \ \ \
auth     |  \\/  ___)| |_)| | | | | || (_| |  ) ) ) )
auth     |   '  |____| .__|_| |_|_| |_\__, | / / / /
auth     |  =========|_|==============|___/=/_/_/_/
auth     |  :: Spring Boot ::        (v2.3.4.RELEASE)
auth     | 
gateway  | 
gateway  |   .   ____          _            __ _ _
gateway  |  /\\ / ___'_ __ _ _(_)_ __  __ _ \ \ \ \
gateway  | ( ( )\___ | '_ | '_| | '_ \/ _` | \ \ \ \
gateway  |  \\/  ___)| |_)| | | | | || (_| |  ) ) ) )
gateway  |   '  |____| .__|_| |_|_| |_\__, | / / / /
gateway  |  =========|_|==============|___/=/_/_/_/
gateway  |  :: Spring Boot ::        (v1.5.4.RELEASE)
gateway  | 
gateway  | 2023-02-26 19:56:27.904  INFO 1 --- [           main] com.ahasan.gatway.ZuulApiGetWayRunner    : No active profile set, falling back to default profiles: default
gateway  | 2023-02-26 19:56:28.107  INFO 1 --- [           main] ationConfigEmbeddedWebApplicationContext : Refreshing org.springframework.boot.context.embedded.AnnotationConfigEmbeddedWebApplicationContext@13c78c0b: startup date [Sun Feb 26 19:56:28 GMT 2023]; parent: org.springframework.context.annotation.AnnotationConfigApplicationContext@50134894
item     | 2023-02-26 19:56:29.342  INFO 1 --- [           main] trationDelegate$BeanPostProcessorChecker : Bean 'org.springframework.cloud.autoconfigure.ConfigurationPropertiesRebinderAutoConfiguration' of type [org.springframework.cloud.autoconfigure.ConfigurationPropertiesRebinderAutoConfiguration$$EnhancerBySpringCGLIB$$333e5399] is not eligible for getting processed by all BeanPostProcessors (for example: not eligible for auto-proxying)
sales    | 2023-02-26 19:56:30.423  INFO 1 --- [           main] trationDelegate$BeanPostProcessorChecker : Bean 'org.springframework.cloud.autoconfigure.ConfigurationPropertiesRebinderAutoConfiguration' of type [org.springframework.cloud.autoconfigure.ConfigurationPropertiesRebinderAutoConfiguration$$EnhancerBySpringCGLIB$$333e5399] is not eligible for getting processed by all BeanPostProcessors (for example: not eligible for auto-proxying)
auth     | 2023-02-26 19:56:30.439  WARN 1 --- [           main] ConfigServletWebServerApplicationContext : Exception encountered during context initialization - cancelling refresh attempt: org.springframework.beans.factory.BeanDefinitionStoreException: Failed to process import candidates for configuration class [com.ahasan.auth.AuthorizationRunner]; nested exception is java.lang.IllegalStateException: Error processing condition on org.springframework.boot.actuate.autoconfigure.audit.AuditEventsEndpointAutoConfiguration
auth     | 2023-02-26 19:56:30.452  INFO 1 --- [           main] ConditionEvaluationReportLoggingListener : 
auth     | 
auth     | Error starting ApplicationContext. To display the conditions report re-run your application with 'debug' enabled.
auth     | 2023-02-26 19:56:30.498 ERROR 1 --- [           main] o.s.boot.SpringApplication               : Application run failed
auth     | 
auth     | org.springframework.beans.factory.BeanDefinitionStoreException: Failed to process import candidates for configuration class [com.ahasan.auth.AuthorizationRunner]; nested exception is java.lang.IllegalStateException: Error processing condition on org.springframework.boot.actuate.autoconfigure.audit.AuditEventsEndpointAutoConfiguration
auth     | 	at org.springframework.context.annotation.ConfigurationClassParser.processImports(ConfigurationClassParser.java:610) ~[spring-context-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
auth     | 	at org.springframework.context.annotation.ConfigurationClassParser.access$800(ConfigurationClassParser.java:111) ~[spring-context-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
auth     | 	at org.springframework.context.annotation.ConfigurationClassParser$DeferredImportSelectorGroupingHandler.lambda$processGroupImports$1(ConfigurationClassParser.java:812) ~[spring-context-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
auth     | 	at java.util.ArrayList.forEach(ArrayList.java:1257) ~[na:1.8.0_212]
auth     | 	at org.springframework.context.annotation.ConfigurationClassParser$DeferredImportSelectorGroupingHandler.processGroupImports(ConfigurationClassParser.java:809) ~[spring-context-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
auth     | 	at org.springframework.context.annotation.ConfigurationClassParser$DeferredImportSelectorHandler.process(ConfigurationClassParser.java:780) ~[spring-context-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
auth     | 	at org.springframework.context.annotation.ConfigurationClassParser.parse(ConfigurationClassParser.java:193) ~[spring-context-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
auth     | 	at org.springframework.context.annotation.ConfigurationClassPostProcessor.processConfigBeanDefinitions(ConfigurationClassPostProcessor.java:319) ~[spring-context-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
auth     | 	at org.springframework.context.annotation.ConfigurationClassPostProcessor.postProcessBeanDefinitionRegistry(ConfigurationClassPostProcessor.java:236) ~[spring-context-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
auth     | 	at org.springframework.context.support.PostProcessorRegistrationDelegate.invokeBeanDefinitionRegistryPostProcessors(PostProcessorRegistrationDelegate.java:280) ~[spring-context-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
auth     | 	at org.springframework.context.support.PostProcessorRegistrationDelegate.invokeBeanFactoryPostProcessors(PostProcessorRegistrationDelegate.java:96) ~[spring-context-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
auth     | 	at org.springframework.context.support.AbstractApplicationContext.invokeBeanFactoryPostProcessors(AbstractApplicationContext.java:707) ~[spring-context-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
auth     | 	at org.springframework.context.support.AbstractApplicationContext.refresh(AbstractApplicationContext.java:533) ~[spring-context-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
auth     | 	at org.springframework.boot.web.servlet.context.ServletWebServerApplicationContext.refresh(ServletWebServerApplicationContext.java:143) ~[spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
auth     | 	at org.springframework.boot.SpringApplication.refresh(SpringApplication.java:758) [spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
auth     | 	at org.springframework.boot.SpringApplication.refresh(SpringApplication.java:750) [spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
auth     | 	at org.springframework.boot.SpringApplication.refreshContext(SpringApplication.java:397) [spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
auth     | 	at org.springframework.boot.SpringApplication.run(SpringApplication.java:315) [spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
auth     | 	at org.springframework.boot.SpringApplication.run(SpringApplication.java:1237) [spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
auth     | 	at org.springframework.boot.SpringApplication.run(SpringApplication.java:1226) [spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
auth     | 	at com.ahasan.auth.AuthorizationRunner.main(AuthorizationRunner.java:14) [classes!/:0.0.1-SNAPSHOT]
auth     | 	at sun.reflect.NativeMethodAccessorImpl.invoke0(Native Method) ~[na:1.8.0_212]
auth     | 	at sun.reflect.NativeMethodAccessorImpl.invoke(NativeMethodAccessorImpl.java:62) ~[na:1.8.0_212]
auth     | 	at sun.reflect.DelegatingMethodAccessorImpl.invoke(DelegatingMethodAccessorImpl.java:43) ~[na:1.8.0_212]
auth     | 	at java.lang.reflect.Method.invoke(Method.java:498) ~[na:1.8.0_212]
auth     | 	at org.springframework.boot.loader.MainMethodRunner.run(MainMethodRunner.java:49) [micro-auth-service-0.0.1-SNAPSHOT.jar:0.0.1-SNAPSHOT]
auth     | 	at org.springframework.boot.loader.Launcher.launch(Launcher.java:107) [micro-auth-service-0.0.1-SNAPSHOT.jar:0.0.1-SNAPSHOT]
auth     | 	at org.springframework.boot.loader.Launcher.launch(Launcher.java:58) [micro-auth-service-0.0.1-SNAPSHOT.jar:0.0.1-SNAPSHOT]
auth     | 	at org.springframework.boot.loader.JarLauncher.main(JarLauncher.java:88) [micro-auth-service-0.0.1-SNAPSHOT.jar:0.0.1-SNAPSHOT]
auth     | Caused by: java.lang.IllegalStateException: Error processing condition on org.springframework.boot.actuate.autoconfigure.audit.AuditEventsEndpointAutoConfiguration
auth     | 	at org.springframework.boot.autoconfigure.condition.SpringBootCondition.matches(SpringBootCondition.java:60) ~[spring-boot-autoconfigure-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
auth     | 	at org.springframework.context.annotation.ConditionEvaluator.shouldSkip(ConditionEvaluator.java:108) ~[spring-context-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
auth     | 	at org.springframework.context.annotation.ConfigurationClassParser.processConfigurationClass(ConfigurationClassParser.java:226) ~[spring-context-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
auth     | 	at org.springframework.context.annotation.ConfigurationClassParser.processImports(ConfigurationClassParser.java:600) ~[spring-context-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
auth     | 	... 28 common frames omitted
auth     | Caused by: java.lang.IllegalArgumentException: Value must only contain valid chars
auth     | 	at org.springframework.util.Assert.isTrue(Assert.java:121) ~[spring-core-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
auth     | 	at org.springframework.boot.actuate.endpoint.EndpointId.<init>(EndpointId.java:58) ~[spring-boot-actuator-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
auth     | 	at org.springframework.boot.actuate.endpoint.EndpointId.fromPropertyValue(EndpointId.java:146) ~[spring-boot-actuator-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
auth     | 	at org.springframework.boot.actuate.autoconfigure.endpoint.expose.IncludeExcludeEndpointFilter$EndpointPatterns.<init>(IncludeExcludeEndpointFilter.java:219) ~[spring-boot-actuator-autoconfigure-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
auth     | 	at org.springframework.boot.actuate.autoconfigure.endpoint.expose.IncludeExcludeEndpointFilter.<init>(IncludeExcludeEndpointFilter.java:89) ~[spring-boot-actuator-autoconfigure-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
auth     | 	at org.springframework.boot.actuate.autoconfigure.endpoint.expose.IncludeExcludeEndpointFilter.<init>(IncludeExcludeEndpointFilter.java:78) ~[spring-boot-actuator-autoconfigure-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
auth     | 	at org.springframework.boot.actuate.autoconfigure.endpoint.condition.OnAvailableEndpointCondition$Exposure.<init>(OnAvailableEndpointCondition.java:96) ~[spring-boot-actuator-autoconfigure-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
auth     | 	at org.springframework.boot.actuate.autoconfigure.endpoint.condition.OnAvailableEndpointCondition.getExposures(OnAvailableEndpointCondition.java:84) ~[spring-boot-actuator-autoconfigure-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
auth     | 	at org.springframework.boot.actuate.autoconfigure.endpoint.condition.OnAvailableEndpointCondition.getMatchOutcome(OnAvailableEndpointCondition.java:64) ~[spring-boot-actuator-autoconfigure-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
auth     | 	at org.springframework.boot.autoconfigure.condition.SpringBootCondition.matches(SpringBootCondition.java:47) ~[spring-boot-autoconfigure-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
auth     | 	... 31 common frames omitted
auth     | 
item     | 
item     |   .   ____          _            __ _ _
item     |  /\\ / ___'_ __ _ _(_)_ __  __ _ \ \ \ \
item     | ( ( )\___ | '_ | '_| | '_ \/ _` | \ \ \ \
item     |  \\/  ___)| |_)| | | | | || (_| |  ) ) ) )
item     |   '  |____| .__|_| |_|_| |_\__, | / / / /
item     |  =========|_|==============|___/=/_/_/_/
item     |  :: Spring Boot ::        (v2.3.4.RELEASE)
item     | 
sales    | 
sales    |   .   ____          _            __ _ _
sales    |  /\\ / ___'_ __ _ _(_)_ __  __ _ \ \ \ \
sales    | ( ( )\___ | '_ | '_| | '_ \/ _` | \ \ \ \
sales    |  \\/  ___)| |_)| | | | | || (_| |  ) ) ) )
sales    |   '  |____| .__|_| |_|_| |_\__, | / / / /
sales    |  =========|_|==============|___/=/_/_/_/
sales    |  :: Spring Boot ::        (v2.3.4.RELEASE)
sales    | 
auth exited with code 1
eureka   | 2023-02-26 19:56:35.995  INFO 1 --- [           main] o.s.cloud.context.scope.GenericScope     : BeanFactory id=7752e73b-1e27-3ee5-b2a7-995bd5297f70
gateway  | 2023-02-26 19:56:36.234  INFO 1 --- [           main] o.s.b.f.s.DefaultListableBeanFactory     : Overriding bean definition for bean 'counterFactory' with a different definition: replacing [Root bean: class [null]; scope=; abstract=false; lazyInit=false; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.cloud.netflix.zuul.ZuulServerAutoConfiguration$ZuulMetricsConfiguration; factoryMethodName=counterFactory; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/cloud/netflix/zuul/ZuulServerAutoConfiguration$ZuulMetricsConfiguration.class]] with [Root bean: class [null]; scope=; abstract=false; lazyInit=false; autowireMode=3; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=org.springframework.cloud.netflix.zuul.ZuulServerAutoConfiguration$ZuulCounterFactoryConfiguration; factoryMethodName=counterFactory; initMethodName=null; destroyMethodName=(inferred); defined in class path resource [org/springframework/cloud/netflix/zuul/ZuulServerAutoConfiguration$ZuulCounterFactoryConfiguration.class]]
eureka   | 2023-02-26 19:56:37.108  INFO 1 --- [           main] trationDelegate$BeanPostProcessorChecker : Bean 'org.springframework.cloud.autoconfigure.ConfigurationPropertiesRebinderAutoConfiguration' of type [org.springframework.cloud.autoconfigure.ConfigurationPropertiesRebinderAutoConfiguration$$EnhancerBySpringCGLIB$$275ea081] is not eligible for getting processed by all BeanPostProcessors (for example: not eligible for auto-proxying)
gateway  | 2023-02-26 19:56:38.424  INFO 1 --- [           main] o.s.cloud.context.scope.GenericScope     : BeanFactory id=d5b7856b-e8fc-3825-97be-e1f7d32e0e47
gateway  | 2023-02-26 19:56:38.813  INFO 1 --- [           main] f.a.AutowiredAnnotationBeanPostProcessor : JSR-330 'javax.inject.Inject' annotation found and supported for autowiring
eureka   | 2023-02-26 19:56:39.330  INFO 1 --- [           main] o.s.b.w.embedded.tomcat.TomcatWebServer  : Tomcat initialized with port(s): 8761 (http)
eureka   | 2023-02-26 19:56:39.422  INFO 1 --- [           main] o.apache.catalina.core.StandardService   : Starting service [Tomcat]
eureka   | 2023-02-26 19:56:39.423  INFO 1 --- [           main] org.apache.catalina.core.StandardEngine  : Starting Servlet engine: [Apache Tomcat/9.0.38]
eureka   | 2023-02-26 19:56:39.740  INFO 1 --- [           main] o.a.c.c.C.[Tomcat].[localhost].[/]       : Initializing Spring embedded WebApplicationContext
eureka   | 2023-02-26 19:56:39.740  INFO 1 --- [           main] w.s.c.ServletWebServerApplicationContext : Root WebApplicationContext: initialization completed in 15129 ms
gateway  | 2023-02-26 19:56:40.898  INFO 1 --- [           main] trationDelegate$BeanPostProcessorChecker : Bean 'org.springframework.cloud.netflix.metrics.MetricsInterceptorConfiguration$MetricsRestTemplateConfiguration' of type [org.springframework.cloud.netflix.metrics.MetricsInterceptorConfiguration$MetricsRestTemplateConfiguration$$EnhancerBySpringCGLIB$$6355d83] is not eligible for getting processed by all BeanPostProcessors (for example: not eligible for auto-proxying)
gateway  | 2023-02-26 19:56:42.215  INFO 1 --- [           main] trationDelegate$BeanPostProcessorChecker : Bean 'org.springframework.cloud.autoconfigure.ConfigurationPropertiesRebinderAutoConfiguration' of type [org.springframework.cloud.autoconfigure.ConfigurationPropertiesRebinderAutoConfiguration$$EnhancerBySpringCGLIB$$f022ba3f] is not eligible for getting processed by all BeanPostProcessors (for example: not eligible for auto-proxying)
eureka   | 2023-02-26 19:56:42.295 ERROR 1 --- [           main] o.s.b.web.embedded.tomcat.TomcatStarter  : Error starting Tomcat context. Exception: org.springframework.beans.factory.UnsatisfiedDependencyException. Message: Error creating bean with name 'traceFilterRegistration' defined in class path resource [org/springframework/cloud/netflix/eureka/server/EurekaServerAutoConfiguration.class]: Unsatisfied dependency expressed through method 'traceFilterRegistration' parameter 0; nested exception is org.springframework.beans.factory.NoSuchBeanDefinitionException: No qualifying bean of type 'javax.servlet.Filter' available: expected at least 1 bean which qualifies as autowire candidate. Dependency annotations: {@org.springframework.beans.factory.annotation.Qualifier(value=httpTraceFilter)}
eureka   | 2023-02-26 19:56:42.506  INFO 1 --- [           main] o.apache.catalina.core.StandardService   : Stopping service [Tomcat]
eureka   | 2023-02-26 19:56:42.523  WARN 1 --- [           main] ConfigServletWebServerApplicationContext : Exception encountered during context initialization - cancelling refresh attempt: org.springframework.context.ApplicationContextException: Unable to start web server; nested exception is org.springframework.boot.web.server.WebServerException: Unable to start embedded Tomcat
eureka   | 2023-02-26 19:56:42.691  INFO 1 --- [           main] ConditionEvaluationReportLoggingListener : 
eureka   | 
eureka   | Error starting ApplicationContext. To display the conditions report re-run your application with 'debug' enabled.
gateway  | 2023-02-26 19:56:46.042  INFO 1 --- [           main] s.b.c.e.t.TomcatEmbeddedServletContainer : Tomcat initialized with port(s): 8180 (http)
eureka   | 2023-02-26 19:56:46.090 ERROR 1 --- [           main] o.s.boot.SpringApplication               : Application run failed
eureka   | 
eureka   | org.springframework.context.ApplicationContextException: Unable to start web server; nested exception is org.springframework.boot.web.server.WebServerException: Unable to start embedded Tomcat
eureka   | 	at org.springframework.boot.web.servlet.context.ServletWebServerApplicationContext.onRefresh(ServletWebServerApplicationContext.java:161) ~[spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
eureka   | 	at org.springframework.context.support.AbstractApplicationContext.refresh(AbstractApplicationContext.java:545) ~[spring-context-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
eureka   | 	at org.springframework.boot.web.servlet.context.ServletWebServerApplicationContext.refresh(ServletWebServerApplicationContext.java:143) ~[spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
eureka   | 	at org.springframework.boot.SpringApplication.refresh(SpringApplication.java:758) [spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
eureka   | 	at org.springframework.boot.SpringApplication.refresh(SpringApplication.java:750) [spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
eureka   | 	at org.springframework.boot.SpringApplication.refreshContext(SpringApplication.java:397) [spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
eureka   | 	at org.springframework.boot.SpringApplication.run(SpringApplication.java:315) [spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
eureka   | 	at org.springframework.boot.SpringApplication.run(SpringApplication.java:1237) [spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
eureka   | 	at org.springframework.boot.SpringApplication.run(SpringApplication.java:1226) [spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
eureka   | 	at com.ahasan.eureka.EurekaServerRunner.main(EurekaServerRunner.java:11) [classes!/:0.0.1-SNAPSHOT]
eureka   | 	at sun.reflect.NativeMethodAccessorImpl.invoke0(Native Method) ~[na:1.8.0_212]
eureka   | 	at sun.reflect.NativeMethodAccessorImpl.invoke(NativeMethodAccessorImpl.java:62) ~[na:1.8.0_212]
eureka   | 	at sun.reflect.DelegatingMethodAccessorImpl.invoke(DelegatingMethodAccessorImpl.java:43) ~[na:1.8.0_212]
eureka   | 	at java.lang.reflect.Method.invoke(Method.java:498) ~[na:1.8.0_212]
eureka   | 	at org.springframework.boot.loader.MainMethodRunner.run(MainMethodRunner.java:49) [micro-eureka-service-0.0.1-SNAPSHOT.jar:0.0.1-SNAPSHOT]
eureka   | 	at org.springframework.boot.loader.Launcher.launch(Launcher.java:107) [micro-eureka-service-0.0.1-SNAPSHOT.jar:0.0.1-SNAPSHOT]
eureka   | 	at org.springframework.boot.loader.Launcher.launch(Launcher.java:58) [micro-eureka-service-0.0.1-SNAPSHOT.jar:0.0.1-SNAPSHOT]
eureka   | 	at org.springframework.boot.loader.JarLauncher.main(JarLauncher.java:88) [micro-eureka-service-0.0.1-SNAPSHOT.jar:0.0.1-SNAPSHOT]
eureka   | Caused by: org.springframework.boot.web.server.WebServerException: Unable to start embedded Tomcat
eureka   | 	at org.springframework.boot.web.embedded.tomcat.TomcatWebServer.initialize(TomcatWebServer.java:142) ~[spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
eureka   | 	at org.springframework.boot.web.embedded.tomcat.TomcatWebServer.<init>(TomcatWebServer.java:104) ~[spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
eureka   | 	at org.springframework.boot.web.embedded.tomcat.TomcatServletWebServerFactory.getTomcatWebServer(TomcatServletWebServerFactory.java:437) ~[spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
eureka   | 	at org.springframework.boot.web.embedded.tomcat.TomcatServletWebServerFactory.getWebServer(TomcatServletWebServerFactory.java:191) ~[spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
eureka   | 	at org.springframework.boot.web.servlet.context.ServletWebServerApplicationContext.createWebServer(ServletWebServerApplicationContext.java:178) ~[spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
eureka   | 	at org.springframework.boot.web.servlet.context.ServletWebServerApplicationContext.onRefresh(ServletWebServerApplicationContext.java:158) ~[spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
eureka   | 	... 17 common frames omitted
eureka   | Caused by: org.springframework.beans.factory.UnsatisfiedDependencyException: Error creating bean with name 'traceFilterRegistration' defined in class path resource [org/springframework/cloud/netflix/eureka/server/EurekaServerAutoConfiguration.class]: Unsatisfied dependency expressed through method 'traceFilterRegistration' parameter 0; nested exception is org.springframework.beans.factory.NoSuchBeanDefinitionException: No qualifying bean of type 'javax.servlet.Filter' available: expected at least 1 bean which qualifies as autowire candidate. Dependency annotations: {@org.springframework.beans.factory.annotation.Qualifier(value=httpTraceFilter)}
eureka   | 	at org.springframework.beans.factory.support.ConstructorResolver.createArgumentArray(ConstructorResolver.java:797) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
eureka   | 	at org.springframework.beans.factory.support.ConstructorResolver.instantiateUsingFactoryMethod(ConstructorResolver.java:538) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
eureka   | 	at org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory.instantiateUsingFactoryMethod(AbstractAutowireCapableBeanFactory.java:1336) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
eureka   | 	at org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory.createBeanInstance(AbstractAutowireCapableBeanFactory.java:1176) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
eureka   | 	at org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory.doCreateBean(AbstractAutowireCapableBeanFactory.java:556) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
eureka   | 	at org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory.createBean(AbstractAutowireCapableBeanFactory.java:516) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
eureka   | 	at org.springframework.beans.factory.support.AbstractBeanFactory.lambda$doGetBean$0(AbstractBeanFactory.java:324) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
eureka   | 	at org.springframework.beans.factory.support.DefaultSingletonBeanRegistry.getSingleton(DefaultSingletonBeanRegistry.java:234) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
eureka   | 	at org.springframework.beans.factory.support.AbstractBeanFactory.doGetBean(AbstractBeanFactory.java:322) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
eureka   | 	at org.springframework.beans.factory.support.AbstractBeanFactory.getBean(AbstractBeanFactory.java:207) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
eureka   | 	at org.springframework.boot.web.servlet.ServletContextInitializerBeans.getOrderedBeansOfType(ServletContextInitializerBeans.java:211) ~[spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
eureka   | 	at org.springframework.boot.web.servlet.ServletContextInitializerBeans.getOrderedBeansOfType(ServletContextInitializerBeans.java:202) ~[spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
eureka   | 	at org.springframework.boot.web.servlet.ServletContextInitializerBeans.addServletContextInitializerBeans(ServletContextInitializerBeans.java:96) ~[spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
eureka   | 	at org.springframework.boot.web.servlet.ServletContextInitializerBeans.<init>(ServletContextInitializerBeans.java:85) ~[spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
eureka   | 	at org.springframework.boot.web.servlet.context.ServletWebServerApplicationContext.getServletContextInitializerBeans(ServletWebServerApplicationContext.java:255) ~[spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
eureka   | 	at org.springframework.boot.web.servlet.context.ServletWebServerApplicationContext.selfInitialize(ServletWebServerApplicationContext.java:229) ~[spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
eureka   | 	at org.springframework.boot.web.embedded.tomcat.TomcatStarter.onStartup(TomcatStarter.java:53) ~[spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
eureka   | 	at org.apache.catalina.core.StandardContext.startInternal(StandardContext.java:5128) ~[tomcat-embed-core-9.0.38.jar!/:9.0.38]
eureka   | 	at org.apache.catalina.util.LifecycleBase.start(LifecycleBase.java:183) ~[tomcat-embed-core-9.0.38.jar!/:9.0.38]
eureka   | 	at org.apache.catalina.core.ContainerBase$StartChild.call(ContainerBase.java:1384) ~[tomcat-embed-core-9.0.38.jar!/:9.0.38]
eureka   | 	at org.apache.catalina.core.ContainerBase$StartChild.call(ContainerBase.java:1374) ~[tomcat-embed-core-9.0.38.jar!/:9.0.38]
eureka   | 	at java.util.concurrent.FutureTask.run(FutureTask.java:266) ~[na:1.8.0_212]
eureka   | 	at org.apache.tomcat.util.threads.InlineExecutorService.execute(InlineExecutorService.java:75) ~[tomcat-embed-core-9.0.38.jar!/:9.0.38]
eureka   | 	at java.util.concurrent.AbstractExecutorService.submit(AbstractExecutorService.java:134) ~[na:1.8.0_212]
eureka   | 	at org.apache.catalina.core.ContainerBase.startInternal(ContainerBase.java:909) ~[tomcat-embed-core-9.0.38.jar!/:9.0.38]
eureka   | 	at org.apache.catalina.core.StandardHost.startInternal(StandardHost.java:843) ~[tomcat-embed-core-9.0.38.jar!/:9.0.38]
eureka   | 	at org.apache.catalina.util.LifecycleBase.start(LifecycleBase.java:183) ~[tomcat-embed-core-9.0.38.jar!/:9.0.38]
eureka   | 	at org.apache.catalina.core.ContainerBase$StartChild.call(ContainerBase.java:1384) ~[tomcat-embed-core-9.0.38.jar!/:9.0.38]
eureka   | 	at org.apache.catalina.core.ContainerBase$StartChild.call(ContainerBase.java:1374) ~[tomcat-embed-core-9.0.38.jar!/:9.0.38]
eureka   | 	at java.util.concurrent.FutureTask.run(FutureTask.java:266) ~[na:1.8.0_212]
eureka   | 	at org.apache.tomcat.util.threads.InlineExecutorService.execute(InlineExecutorService.java:75) ~[tomcat-embed-core-9.0.38.jar!/:9.0.38]
eureka   | 	at java.util.concurrent.AbstractExecutorService.submit(AbstractExecutorService.java:134) ~[na:1.8.0_212]
eureka   | 	at org.apache.catalina.core.ContainerBase.startInternal(ContainerBase.java:909) ~[tomcat-embed-core-9.0.38.jar!/:9.0.38]
eureka   | 	at org.apache.catalina.core.StandardEngine.startInternal(StandardEngine.java:262) ~[tomcat-embed-core-9.0.38.jar!/:9.0.38]
eureka   | 	at org.apache.catalina.util.LifecycleBase.start(LifecycleBase.java:183) ~[tomcat-embed-core-9.0.38.jar!/:9.0.38]
eureka   | 	at org.apache.catalina.core.StandardService.startInternal(StandardService.java:421) ~[tomcat-embed-core-9.0.38.jar!/:9.0.38]
eureka   | 	at org.apache.catalina.util.LifecycleBase.start(LifecycleBase.java:183) ~[tomcat-embed-core-9.0.38.jar!/:9.0.38]
eureka   | 	at org.apache.catalina.core.StandardServer.startInternal(StandardServer.java:930) ~[tomcat-embed-core-9.0.38.jar!/:9.0.38]
eureka   | 	at org.apache.catalina.util.LifecycleBase.start(LifecycleBase.java:183) ~[tomcat-embed-core-9.0.38.jar!/:9.0.38]
eureka   | 	at org.apache.catalina.startup.Tomcat.start(Tomcat.java:486) ~[tomcat-embed-core-9.0.38.jar!/:9.0.38]
eureka   | 	at org.springframework.boot.web.embedded.tomcat.TomcatWebServer.initialize(TomcatWebServer.java:123) ~[spring-boot-2.3.4.RELEASE.jar!/:2.3.4.RELEASE]
eureka   | 	... 22 common frames omitted
eureka   | Caused by: org.springframework.beans.factory.NoSuchBeanDefinitionException: No qualifying bean of type 'javax.servlet.Filter' available: expected at least 1 bean which qualifies as autowire candidate. Dependency annotations: {@org.springframework.beans.factory.annotation.Qualifier(value=httpTraceFilter)}
eureka   | 	at org.springframework.beans.factory.support.DefaultListableBeanFactory.raiseNoMatchingBeanFound(DefaultListableBeanFactory.java:1717) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
eureka   | 	at org.springframework.beans.factory.support.DefaultListableBeanFactory.doResolveDependency(DefaultListableBeanFactory.java:1273) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
eureka   | 	at org.springframework.beans.factory.support.DefaultListableBeanFactory.resolveDependency(DefaultListableBeanFactory.java:1227) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
eureka   | 	at org.springframework.beans.factory.support.ConstructorResolver.resolveAutowiredArgument(ConstructorResolver.java:884) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
eureka   | 	at org.springframework.beans.factory.support.ConstructorResolver.createArgumentArray(ConstructorResolver.java:788) ~[spring-beans-5.2.9.RELEASE.jar!/:5.2.9.RELEASE]
eureka   | 	... 62 common frames omitted
eureka   | 
gateway  | 2023-02-26 19:56:46.137  INFO 1 --- [           main] o.apache.catalina.core.StandardService   : Starting service [Tomcat]
gateway  | 2023-02-26 19:56:46.195  INFO 1 --- [           main] org.apache.catalina.core.StandardEngine  : Starting Servlet Engine: Apache Tomcat/8.5.15
eureka exited with code 1
gateway  | 2023-02-26 19:56:46.925  INFO 1 --- [ost-startStop-1] o.a.c.c.C.[Tomcat].[localhost].[/]       : Initializing Spring embedded WebApplicationContext
gateway  | 2023-02-26 19:56:46.925  INFO 1 --- [ost-startStop-1] o.s.web.context.ContextLoader            : Root WebApplicationContext: initialization completed in 18818 ms
gateway  | 2023-02-26 19:56:53.009  INFO 1 --- [ost-startStop-1] o.s.b.w.servlet.ServletRegistrationBean  : Mapping servlet: 'proxyStreamServlet' to [/proxy.stream]
gateway  | 2023-02-26 19:56:53.012  INFO 1 --- [ost-startStop-1] o.s.b.w.servlet.ServletRegistrationBean  : Mapping servlet: 'dispatcherServlet' to [/]
gateway  | 2023-02-26 19:56:53.087  INFO 1 --- [ost-startStop-1] o.s.b.w.servlet.ServletRegistrationBean  : Mapping servlet: 'zuulServlet' to [/zuul/*]
gateway  | 2023-02-26 19:56:53.100  INFO 1 --- [ost-startStop-1] o.s.b.w.servlet.FilterRegistrationBean   : Mapping filter: 'CORSFilter' to: [/*]
gateway  | 2023-02-26 19:56:53.102  INFO 1 --- [ost-startStop-1] o.s.b.w.servlet.FilterRegistrationBean   : Mapping filter: 'metricsFilter' to: [/*]
gateway  | 2023-02-26 19:56:53.103  INFO 1 --- [ost-startStop-1] o.s.b.w.servlet.FilterRegistrationBean   : Mapping filter: 'characterEncodingFilter' to: [/*]
gateway  | 2023-02-26 19:56:53.103  INFO 1 --- [ost-startStop-1] o.s.b.w.servlet.FilterRegistrationBean   : Mapping filter: 'hiddenHttpMethodFilter' to: [/*]
gateway  | 2023-02-26 19:56:53.104  INFO 1 --- [ost-startStop-1] o.s.b.w.servlet.FilterRegistrationBean   : Mapping filter: 'httpPutFormContentFilter' to: [/*]
gateway  | 2023-02-26 19:56:53.105  INFO 1 --- [ost-startStop-1] o.s.b.w.servlet.FilterRegistrationBean   : Mapping filter: 'requestContextFilter' to: [/*]
gateway  | 2023-02-26 19:56:53.105  INFO 1 --- [ost-startStop-1] o.s.b.w.servlet.FilterRegistrationBean   : Mapping filter: 'webRequestLoggingFilter' to: [/*]
gateway  | 2023-02-26 19:56:53.106  INFO 1 --- [ost-startStop-1] o.s.b.w.servlet.FilterRegistrationBean   : Mapping filter: 'applicationContextIdFilter' to: [/*]
gateway  | 2023-02-26 19:56:53.302  INFO 1 --- [ost-startStop-1] com.ahasan.gatway.filters.CORSFilter     : Implementation not required
gateway  | 2023-02-26 19:56:54.114  INFO 1 --- [           main] o.s.ui.freemarker.SpringTemplateLoader   : SpringTemplateLoader for FreeMarker: using resource loader [org.springframework.boot.context.embedded.AnnotationConfigEmbeddedWebApplicationContext@13c78c0b: startup date [Sun Feb 26 19:56:28 GMT 2023]; parent: org.springframework.context.annotation.AnnotationConfigApplicationContext@50134894] and template loader path [classpath:/templates/]
gateway  | 2023-02-26 19:56:54.116  INFO 1 --- [           main] o.s.w.s.v.f.FreeMarkerConfigurer         : ClassTemplateLoader for Spring macros added to FreeMarker configuration
gateway  | 2023-02-26 19:56:54.712  WARN 1 --- [           main] c.n.c.sources.URLConfigurationSource     : No URLs will be polled as dynamic configuration sources.
gateway  | 2023-02-26 19:56:54.712  INFO 1 --- [           main] c.n.c.sources.URLConfigurationSource     : To enable URLs as dynamic configuration sources, define System property archaius.configurationSource.additionalUrls or make config.properties available on classpath.
gateway  | 2023-02-26 19:56:54.798  INFO 1 --- [           main] c.netflix.config.DynamicPropertyFactory  : DynamicPropertyFactory is initialized with configuration sources: com.netflix.config.ConcurrentCompositeConfiguration@3bf9ce3e
item     | 2023-02-26 19:56:56.788  WARN 1 --- [           main] c.n.c.sources.URLConfigurationSource     : No URLs will be polled as dynamic configuration sources.
item     | 2023-02-26 19:56:57.698  WARN 1 --- [           main] o.s.boot.actuate.endpoint.EndpointId     : Endpoint ID 'service-registry' contains invalid characters, please migrate to a valid format.
gateway  | 2023-02-26 19:56:58.318  WARN 1 --- [           main] c.n.c.sources.URLConfigurationSource     : No URLs will be polled as dynamic configuration sources.
gateway  | 2023-02-26 19:56:58.318  INFO 1 --- [           main] c.n.c.sources.URLConfigurationSource     : To enable URLs as dynamic configuration sources, define System property archaius.configurationSource.additionalUrls or make config.properties available on classpath.
sales    | 2023-02-26 19:57:00.418  WARN 1 --- [           main] c.n.c.sources.URLConfigurationSource     : No URLs will be polled as dynamic configuration sources.
sales    | 2023-02-26 19:57:01.224  WARN 1 --- [           main] o.s.boot.actuate.endpoint.EndpointId     : Endpoint ID 'service-registry' contains invalid characters, please migrate to a valid format.
gateway  | 2023-02-26 19:57:01.324  INFO 1 --- [           main] s.w.s.m.m.a.RequestMappingHandlerAdapter : Looking for @ControllerAdvice: org.springframework.boot.context.embedded.AnnotationConfigEmbeddedWebApplicationContext@13c78c0b: startup date [Sun Feb 26 19:56:28 GMT 2023]; parent: org.springframework.context.annotation.AnnotationConfigApplicationContext@50134894
gateway  | 2023-02-26 19:57:02.417  INFO 1 --- [           main] s.w.s.m.m.a.RequestMappingHandlerMapping : Mapped "{[/hystrix/{path}]}" onto public java.lang.String org.springframework.cloud.netflix.hystrix.dashboard.HystrixDashboardController.monitor(java.lang.String,org.springframework.ui.Model,org.springframework.web.context.request.WebRequest)
gateway  | 2023-02-26 19:57:02.495  INFO 1 --- [           main] s.w.s.m.m.a.RequestMappingHandlerMapping : Mapped "{[/hystrix]}" onto public java.lang.String org.springframework.cloud.netflix.hystrix.dashboard.HystrixDashboardController.home(org.springframework.ui.Model,org.springframework.web.context.request.WebRequest)
gateway  | 2023-02-26 19:57:02.503  INFO 1 --- [           main] s.w.s.m.m.a.RequestMappingHandlerMapping : Mapped "{[/error],produces=[text/html]}" onto public org.springframework.web.servlet.ModelAndView org.springframework.boot.autoconfigure.web.BasicErrorController.errorHtml(javax.servlet.http.HttpServletRequest,javax.servlet.http.HttpServletResponse)
gateway  | 2023-02-26 19:57:02.504  INFO 1 --- [           main] s.w.s.m.m.a.RequestMappingHandlerMapping : Mapped "{[/error]}" onto public org.springframework.http.ResponseEntity<java.util.Map<java.lang.String, java.lang.Object>> org.springframework.boot.autoconfigure.web.BasicErrorController.error(javax.servlet.http.HttpServletRequest)
gateway  | 2023-02-26 19:57:03.100  INFO 1 --- [           main] o.s.w.s.handler.SimpleUrlHandlerMapping  : Mapped URL path [/webjars/**] onto handler of type [class org.springframework.web.servlet.resource.ResourceHttpRequestHandler]
gateway  | 2023-02-26 19:57:03.100  INFO 1 --- [           main] o.s.w.s.handler.SimpleUrlHandlerMapping  : Mapped URL path [/**] onto handler of type [class org.springframework.web.servlet.resource.ResourceHttpRequestHandler]
gateway  | 2023-02-26 19:57:03.604  INFO 1 --- [           main] o.s.w.s.handler.SimpleUrlHandlerMapping  : Mapped URL path [/**/favicon.ico] onto handler of type [class org.springframework.web.servlet.resource.ResourceHttpRequestHandler]
item     | 2023-02-26 19:57:05.321  WARN 1 --- [           main] JpaBaseConfiguration$JpaWebConfiguration : spring.jpa.open-in-view is enabled by default. Therefore, database queries may be performed during view rendering. Explicitly configure spring.jpa.open-in-view to disable this warning
sales    | 2023-02-26 19:57:07.602  WARN 1 --- [           main] JpaBaseConfiguration$JpaWebConfiguration : spring.jpa.open-in-view is enabled by default. Therefore, database queries may be performed during view rendering. Explicitly configure spring.jpa.open-in-view to disable this warning
gateway  | 2023-02-26 19:57:11.424  INFO 1 --- [           main] o.s.b.a.e.mvc.EndpointHandlerMapping     : Mapped "{[/configprops || /configprops.json],methods=[GET],produces=[application/vnd.spring-boot.actuator.v1+json || application/json]}" onto public java.lang.Object org.springframework.boot.actuate.endpoint.mvc.EndpointMvcAdapter.invoke()
gateway  | 2023-02-26 19:57:11.424  INFO 1 --- [           main] o.s.b.a.e.mvc.EndpointHandlerMapping     : Mapped "{[/features || /features.json],methods=[GET],produces=[application/vnd.spring-boot.actuator.v1+json || application/json]}" onto public java.lang.Object org.springframework.boot.actuate.endpoint.mvc.EndpointMvcAdapter.invoke()
gateway  | 2023-02-26 19:57:11.425  INFO 1 --- [           main] o.s.b.a.e.mvc.EndpointHandlerMapping     : Mapped "{[/beans || /beans.json],methods=[GET],produces=[application/vnd.spring-boot.actuator.v1+json || application/json]}" onto public java.lang.Object org.springframework.boot.actuate.endpoint.mvc.EndpointMvcAdapter.invoke()
gateway  | 2023-02-26 19:57:11.494  INFO 1 --- [           main] o.s.b.a.e.mvc.EndpointHandlerMapping     : Mapped "{[/health || /health.json],methods=[GET],produces=[application/vnd.spring-boot.actuator.v1+json || application/json]}" onto public java.lang.Object org.springframework.boot.actuate.endpoint.mvc.HealthMvcEndpoint.invoke(javax.servlet.http.HttpServletRequest,java.security.Principal)
gateway  | 2023-02-26 19:57:11.496  INFO 1 --- [           main] o.s.b.a.e.mvc.EndpointHandlerMapping     : Mapped "{[/env/{name:.*}],methods=[GET],produces=[application/vnd.spring-boot.actuator.v1+json || application/json]}" onto public java.lang.Object org.springframework.boot.actuate.endpoint.mvc.EnvironmentMvcEndpoint.value(java.lang.String)
gateway  | 2023-02-26 19:57:11.497  INFO 1 --- [           main] o.s.b.a.e.mvc.EndpointHandlerMapping     : Mapped "{[/env || /env.json],methods=[GET],produces=[application/vnd.spring-boot.actuator.v1+json || application/json]}" onto public java.lang.Object org.springframework.boot.actuate.endpoint.mvc.EndpointMvcAdapter.invoke()
gateway  | 2023-02-26 19:57:11.502  INFO 1 --- [           main] o.s.b.a.e.mvc.EndpointHandlerMapping     : Mapped "{[/routes || /routes.json],methods=[GET],params=[format],produces=[application/vnd.spring-boot.actuator.v1+json || application/json]}" onto public java.lang.Object org.springframework.cloud.netflix.zuul.RoutesMvcEndpoint.invokeRouteDetails(java.lang.String)
gateway  | 2023-02-26 19:57:11.502  INFO 1 --- [           main] o.s.b.a.e.mvc.EndpointHandlerMapping     : Mapped "{[/routes || /routes.json],methods=[POST]}" onto public java.lang.Object org.springframework.cloud.netflix.zuul.RoutesMvcEndpoint.reset()
gateway  | 2023-02-26 19:57:11.502  INFO 1 --- [           main] o.s.b.a.e.mvc.EndpointHandlerMapping     : Mapped "{[/routes || /routes.json],methods=[GET],produces=[application/vnd.spring-boot.actuator.v1+json || application/json]}" onto public java.lang.Object org.springframework.boot.actuate.endpoint.mvc.EndpointMvcAdapter.invoke()
gateway  | 2023-02-26 19:57:11.505  INFO 1 --- [           main] o.s.b.a.e.mvc.EndpointHandlerMapping     : Mapped "{[/info || /info.json],methods=[GET],produces=[application/vnd.spring-boot.actuator.v1+json || application/json]}" onto public java.lang.Object org.springframework.boot.actuate.endpoint.mvc.EndpointMvcAdapter.invoke()
gateway  | 2023-02-26 19:57:11.512  INFO 1 --- [           main] o.s.b.a.e.mvc.EndpointHandlerMapping     : Mapped "{[/dump || /dump.json],methods=[GET],produces=[application/vnd.spring-boot.actuator.v1+json || application/json]}" onto public java.lang.Object org.springframework.boot.actuate.endpoint.mvc.EndpointMvcAdapter.invoke()
gateway  | 2023-02-26 19:57:11.513  INFO 1 --- [           main] o.s.b.a.e.mvc.EndpointHandlerMapping     : Mapped "{[/service-registry/instance-status],methods=[POST]}" onto public org.springframework.http.ResponseEntity<?> org.springframework.cloud.client.serviceregistry.endpoint.ServiceRegistryEndpoint.setStatus(java.lang.String)
gateway  | 2023-02-26 19:57:11.514  INFO 1 --- [           main] o.s.b.a.e.mvc.EndpointHandlerMapping     : Mapped "{[/service-registry/instance-status],methods=[GET]}" onto public org.springframework.http.ResponseEntity org.springframework.cloud.client.serviceregistry.endpoint.ServiceRegistryEndpoint.getStatus()
gateway  | 2023-02-26 19:57:11.515  INFO 1 --- [           main] o.s.b.a.e.mvc.EndpointHandlerMapping     : Mapped "{[/autoconfig || /autoconfig.json],methods=[GET],produces=[application/vnd.spring-boot.actuator.v1+json || application/json]}" onto public java.lang.Object org.springframework.boot.actuate.endpoint.mvc.EndpointMvcAdapter.invoke()
gateway  | 2023-02-26 19:57:11.516  INFO 1 --- [           main] o.s.b.a.e.mvc.EndpointHandlerMapping     : Mapped "{[/mappings || /mappings.json],methods=[GET],produces=[application/vnd.spring-boot.actuator.v1+json || application/json]}" onto public java.lang.Object org.springframework.boot.actuate.endpoint.mvc.EndpointMvcAdapter.invoke()
gateway  | 2023-02-26 19:57:11.518  INFO 1 --- [           main] o.s.b.a.e.mvc.EndpointHandlerMapping     : Mapped "{[/heapdump || /heapdump.json],methods=[GET],produces=[application/octet-stream]}" onto public void org.springframework.boot.actuate.endpoint.mvc.HeapdumpMvcEndpoint.invoke(boolean,javax.servlet.http.HttpServletRequest,javax.servlet.http.HttpServletResponse) throws java.io.IOException,javax.servlet.ServletException
gateway  | 2023-02-26 19:57:11.519  INFO 1 --- [           main] o.s.b.a.e.mvc.EndpointHandlerMapping     : Mapped "{[/auditevents || /auditevents.json],methods=[GET],produces=[application/vnd.spring-boot.actuator.v1+json || application/json]}" onto public org.springframework.http.ResponseEntity<?> org.springframework.boot.actuate.endpoint.mvc.AuditEventsMvcEndpoint.findByPrincipalAndAfterAndType(java.lang.String,java.util.Date,java.lang.String)
gateway  | 2023-02-26 19:57:11.520  INFO 1 --- [           main] o.s.b.a.e.mvc.EndpointHandlerMapping     : Mapped "{[/refresh || /refresh.json],methods=[POST]}" onto public java.lang.Object org.springframework.cloud.endpoint.GenericPostableMvcEndpoint.invoke()
gateway  | 2023-02-26 19:57:11.522  INFO 1 --- [           main] o.s.b.a.e.mvc.EndpointHandlerMapping     : Mapped "{[/loggers/{name:.*}],methods=[GET],produces=[application/vnd.spring-boot.actuator.v1+json || application/json]}" onto public java.lang.Object org.springframework.boot.actuate.endpoint.mvc.LoggersMvcEndpoint.get(java.lang.String)
gateway  | 2023-02-26 19:57:11.523  INFO 1 --- [           main] o.s.b.a.e.mvc.EndpointHandlerMapping     : Mapped "{[/loggers/{name:.*}],methods=[POST],consumes=[application/vnd.spring-boot.actuator.v1+json || application/json],produces=[application/vnd.spring-boot.actuator.v1+json || application/json]}" onto public java.lang.Object org.springframework.boot.actuate.endpoint.mvc.LoggersMvcEndpoint.set(java.lang.String,java.util.Map<java.lang.String, java.lang.String>)
gateway  | 2023-02-26 19:57:11.523  INFO 1 --- [           main] o.s.b.a.e.mvc.EndpointHandlerMapping     : Mapped "{[/loggers || /loggers.json],methods=[GET],produces=[application/vnd.spring-boot.actuator.v1+json || application/json]}" onto public java.lang.Object org.springframework.boot.actuate.endpoint.mvc.EndpointMvcAdapter.invoke()
gateway  | 2023-02-26 19:57:11.591  INFO 1 --- [           main] o.s.b.a.e.mvc.EndpointHandlerMapping     : Mapped "{[/env],methods=[POST]}" onto public java.lang.Object org.springframework.cloud.context.environment.EnvironmentManagerMvcEndpoint.value(java.util.Map<java.lang.String, java.lang.String>)
gateway  | 2023-02-26 19:57:11.594  INFO 1 --- [           main] o.s.b.a.e.mvc.EndpointHandlerMapping     : Mapped "{[/env/reset],methods=[POST]}" onto public java.util.Map<java.lang.String, java.lang.Object> org.springframework.cloud.context.environment.EnvironmentManagerMvcEndpoint.reset()
gateway  | 2023-02-26 19:57:11.596  INFO 1 --- [           main] o.s.b.a.e.mvc.EndpointHandlerMapping     : Mapped "{[/filters || /filters.json],methods=[GET],produces=[application/vnd.spring-boot.actuator.v1+json || application/json]}" onto public java.lang.Object org.springframework.boot.actuate.endpoint.mvc.EndpointMvcAdapter.invoke()
gateway  | 2023-02-26 19:57:11.596  INFO 1 --- [           main] o.s.b.a.e.mvc.EndpointHandlerMapping     : Mapped "{[/hystrix.stream/**]}" onto public org.springframework.web.servlet.ModelAndView org.springframework.cloud.netflix.endpoint.ServletWrappingEndpoint.handle(javax.servlet.http.HttpServletRequest,javax.servlet.http.HttpServletResponse) throws java.lang.Exception
gateway  | 2023-02-26 19:57:11.597  INFO 1 --- [           main] o.s.b.a.e.mvc.EndpointHandlerMapping     : Mapped "{[/archaius || /archaius.json],methods=[GET],produces=[application/vnd.spring-boot.actuator.v1+json || application/json]}" onto public java.lang.Object org.springframework.boot.actuate.endpoint.mvc.EndpointMvcAdapter.invoke()
gateway  | 2023-02-26 19:57:11.599  INFO 1 --- [           main] o.s.b.a.e.mvc.EndpointHandlerMapping     : Mapped "{[/metrics/{name:.*}],methods=[GET],produces=[application/vnd.spring-boot.actuator.v1+json || application/json]}" onto public java.lang.Object org.springframework.boot.actuate.endpoint.mvc.MetricsMvcEndpoint.value(java.lang.String)
gateway  | 2023-02-26 19:57:11.599  INFO 1 --- [           main] o.s.b.a.e.mvc.EndpointHandlerMapping     : Mapped "{[/metrics || /metrics.json],methods=[GET],produces=[application/vnd.spring-boot.actuator.v1+json || application/json]}" onto public java.lang.Object org.springframework.boot.actuate.endpoint.mvc.EndpointMvcAdapter.invoke()
gateway  | 2023-02-26 19:57:11.600  INFO 1 --- [           main] o.s.b.a.e.mvc.EndpointHandlerMapping     : Mapped "{[/trace || /trace.json],methods=[GET],produces=[application/vnd.spring-boot.actuator.v1+json || application/json]}" onto public java.lang.Object org.springframework.boot.actuate.endpoint.mvc.EndpointMvcAdapter.invoke()
item     | 2023-02-26 19:57:11.624  WARN 1 --- [         task-1] com.zaxxer.hikari.util.DriverDataSource  : Registered driver with driverClassName=com.mysql.jdbc.Driver was not found, trying direct instantiation.
item     | 2023-02-26 19:57:12.494  WARN 1 --- [           main] ConfigServletWebServerApplicationContext : Exception encountered during context initialization - cancelling refresh attempt: org.springframework.beans.factory.UnsatisfiedDependencyException: Error creating bean with name 'documentationPluginsBootstrapper' defined in URL [jar:file:/micro-item-service-0.0.1-SNAPSHOT.jar!/BOOT-INF/lib/springfox-spring-web-2.9.2.jar!/springfox/documentation/spring/web/plugins/DocumentationPluginsBootstrapper.class]: Unsatisfied dependency expressed through constructor parameter 1; nested exception is org.springframework.beans.factory.UnsatisfiedDependencyException: Error creating bean with name 'webMvcRequestHandlerProvider' defined in URL [jar:file:/micro-item-service-0.0.1-SNAPSHOT.jar!/BOOT-INF/lib/springfox-spring-web-2.9.2.jar!/springfox/documentation/spring/web/plugins/WebMvcRequestHandlerProvider.class]: Unsatisfied dependency expressed through constructor parameter 1; nested exception is org.springframework.beans.factory.BeanCreationException: Error creating bean with name 'webEndpointServletHandlerMapping' defined in class path resource [org/springframework/boot/actuate/autoconfigure/endpoint/web/servlet/WebMvcEndpointManagementContextConfiguration.class]: Bean instantiation via factory method failed; nested exception is org.springframework.beans.BeanInstantiationException: Failed to instantiate [org.springframework.boot.actuate.endpoint.web.servlet.WebMvcEndpointHandlerMapping]: Factory method 'webEndpointServletHandlerMapping' threw exception; nested exception is org.springframework.beans.factory.UnsatisfiedDependencyException: Error creating bean with name 'healthEndpoint' defined in class path resource [org/springframework/boot/actuate/autoconfigure/health/HealthEndpointConfiguration.class]: Unsatisfied dependency expressed through method 'healthEndpoint' parameter 0; nested exception is org.springframework.beans.factory.BeanCreationException: Error creating bean with name 'healthContributorRegistry' defined in class path resource [org/springframework/boot/actuate/autoconfigure/health/HealthEndpointConfiguration.class]: Bean instantiation via factory method failed; nested exception is org.springframework.beans.BeanInstantiationException: Failed to instantiate [org.springframework.boot.actuate.health.HealthContributorRegistry]: Factory method 'healthContributorRegistry' threw exception; nested exception is org.springframework.beans.factory.BeanCreationException: Error creating bean with name 'discoveryCompositeHealthIndicator' defined in class path resource [org/springframework/cloud/client/CommonsClientAutoConfiguration$DiscoveryLoadBalancerConfiguration.class]: Bean instantiation via factory method failed; nested exception is org.springframework.beans.BeanInstantiationException: Failed to instantiate [org.springframework.cloud.client.discovery.health.DiscoveryCompositeHealthIndicator]: Factory method 'discoveryCompositeHealthIndicator' threw exception; nested exception is java.lang.NoSuchMethodError: org.springframework.boot.actuate.health.CompositeHealthIndicator.<init>(Lorg/springframework/boot/actuate/health/HealthAggregator;)V

```
