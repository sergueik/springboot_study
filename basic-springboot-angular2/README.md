### Info

Replica of [Angular2-springboot](https://github.com/arifcseru/angular-springboot-crud)
with intent to get SQLite to replace H2, downgraded to Spring Boot __1.5.4__

### Build

#### Skip
if plugin run failed fallback to  manual download `node.js` of relevant version from `https://nodejs.org/download/`:

```cmd
curl -O https://nodejs.org/download/release/latest-v9.x/node-v9.11.2-win-x64.zip
```
```cmd
unzip.exe node-v9.11.2-win-x64.zip
path=%path%;%CD%\node-v9.11.2-win-x64
```
create empty static resource dir
```cmd
mkdir -p src/main/resources/static
touch !$/.gitkeep
```

build manually:

```cmd
cd angular_app
call npm.cmd install
call npx.cmd ng build --configuration production
```

it should not be necessary, use plugin
* the npm install operations to maven `pom.xml`:
```xml
      <plugin>
        <groupId>com.github.eirslett</groupId>
        <artifactId>frontend-maven-plugin</artifactId>
        <version>${frontend-maven-plugin.version}</version>
        <configuration>
          <nodeVersion>v9.11.2</nodeVersion>
          <workingDirectory>angular_app</workingDirectory>
          <installDirectory>src/main/resources/static</installDirectory>
        </configuration>
        <executions>
          <execution>
            <id>install-npm</id>
            <goals>
              <goal>install-node-and-npm</goal>
            </goals>
          </execution>
          <execution>
            <id>npm-install</id>
            <goals>
              <goal>npm</goal>
            </goals>
          </execution>
          <execution>
            <id>npm run build</id>
            <goals>
              <goal>npm</goal>
            </goals>
            <configuration>
              <arguments>run build</arguments>
            </configuration>
          </execution>
        </executions>
      </plugin>
```
```text
Caused by: java.lang.IllegalStateException: Unsupported type 17 [1.4.200/3]juna
```

remove db and make it rebuild
alternatively use `ant-run` version __1.8__ of the plugin. The later version needs switch from `<tasks>` to `<target>`...

### Package nd Launch

``` cmd
mvn package
mvn spring-boot:run
```

### Note

if you see error running the tests 
```txt
java.lang.ClassNotFoundException: javax.xml.bind.JAXBException
```
your version of Java is too new:

```cmd
java -version
```
```text
openjdk version "11.0.12" 2021-07-20 LTS
```

will need to [apply patch](https://stackoverflow.com/questions/43574426/how-to-resolve-java-lang-noclassdeffounderror-javax-xml-bind-jaxbexception) the `pom.xml`:
```xml
 <dependency>
   <groupId>jakarta.xml.bind</groupId>
   <artifactId>jakarta.xml.bind-api</artifactId>
   <version>2.3.2</version>
 </dependency>
 <dependency>
   <groupId>org.glassfish.jaxb</groupId>
   <artifactId>jaxb-runtime</artifactId>
   <version>2.3.2</version>
 </dependency>

```


if using Java 11 or newer and migrating to Jakarta XML Binding, note the package name change from `javax.xml.bind` to `jakarta.xml.bind` 

* access app on`http://localhost:8084/`
* access H2 on `http://localhost:8084/user_db`


![app](https://github.com/sergueik/springboot_study/blob/master/basic-springboot-angular2/screenshots/app.png)


![app2](https://github.com/sergueik/springboot_study/blob/master/basic-springboot-angular2/screenshots/app2.png)

![database](https://github.com/sergueik/springboot_study/blob/master/basic-springboot-angular2/screenshots/h2console.png)

### NOTE

seeing the error in runtime (no attempt to cure):

```text
  .   ____          _            __ _ _
 /\\ / ___'_ __ _ _(_)_ __  __ _ \ \ \ \
( ( )\___ | '_ | '_| | '_ \/ _` | \ \ \ \
 \\/  ___)| |_)| | | | | || (_| |  ) ) ) )
  '  |____| .__|_| |_|_| |_\__, | / / / /
 =========|_|==============|___/=/_/_/_/
 :: Spring Boot ::        (v1.5.4.RELEASE)

2025-07-08 19:30:52.124  INFO 33688 --- [           main] example.CrudBackendApplication           : Starting CrudBackendApplication on sergueik23 with PID 33688 (C:\developer\sergueik\springboot_study\basic-springboot-angular2\target\classes started by kouzm in C:\developer\sergueik\springboot_study\basic-springboot-angular2)
2025-07-08 19:30:52.126  INFO 33688 --- [           main] example.CrudBackendApplication           : No active profile set, falling back to default profiles: default
2025-07-08 19:30:52.177  INFO 33688 --- [           main] ationConfigEmbeddedWebApplicationContext : Refreshing org.springframework.boot.context.embedded.AnnotationConfigEmbeddedWebApplicationContext@5adef0ac: startup date [Tue Jul 08 19:30:52 EDT 2025]; root of context hierarchy
WARNING: An illegal reflective access operation has occurred
WARNING: Illegal reflective access by org.springframework.cglib.core.ReflectUtils$1 (file:/C:/Users/kouzm/.m2/repository/org/springframework/spring-core/4.3.9.RELEASE/spring-core-4.3.9.RELEASE.jar) to method java.lang.ClassLoader.defineClass(java.lang.String,byte[],int,int,java.security.ProtectionDomain)
WARNING: Please consider reporting this to the maintainers of org.springframework.cglib.core.ReflectUtils$1
WARNING: Use --illegal-access=warn to enable warnings of further illegal reflective access operations
WARNING: All illegal access operations will be denied in a future release
2025-07-08 19:30:53.617  INFO 33688 --- [           main] s.b.c.e.t.TomcatEmbeddedServletContainer : Tomcat initialized with port(s): 8084 (http)
2025-07-08 19:30:53.631  INFO 33688 --- [           main] o.apache.catalina.core.StandardService   : Starting service [Tomcat]
2025-07-08 19:30:53.633  INFO 33688 --- [           main] org.apache.catalina.core.StandardEngine  : Starting Servlet Engine: Apache Tomcat/8.5.15
2025-07-08 19:30:53.744  INFO 33688 --- [ost-startStop-1] o.a.c.c.C.[Tomcat].[localhost].[/]       : Initializing Spring embedded WebApplicationContext
2025-07-08 19:30:53.746  INFO 33688 --- [ost-startStop-1] o.s.web.context.ContextLoader            : Root WebApplicationContext: initialization completed in 1575 ms
2025-07-08 19:30:53.876  INFO 33688 --- [ost-startStop-1] o.s.b.w.servlet.ServletRegistrationBean  : Mapping servlet: 'dispatcherServlet' to [/]
2025-07-08 19:30:53.879  INFO 33688 --- [ost-startStop-1] o.s.b.w.servlet.ServletRegistrationBean  : Mapping servlet: 'webServlet' to [/user_db/*]
2025-07-08 19:30:53.882  INFO 33688 --- [ost-startStop-1] o.s.b.w.servlet.FilterRegistrationBean   : Mapping filter: 'characterEncodingFilter' to: [/*]
2025-07-08 19:30:53.883  INFO 33688 --- [ost-startStop-1] o.s.b.w.servlet.FilterRegistrationBean   : Mapping filter: 'hiddenHttpMethodFilter' to: [/*]
2025-07-08 19:30:53.883  INFO 33688 --- [ost-startStop-1] o.s.b.w.servlet.FilterRegistrationBean   : Mapping filter: 'httpPutFormContentFilter' to: [/*]
2025-07-08 19:30:53.883  INFO 33688 --- [ost-startStop-1] o.s.b.w.servlet.FilterRegistrationBean   : Mapping filter: 'requestContextFilter' to: [/*]
2025-07-08 19:30:54.313  INFO 33688 --- [           main] j.LocalContainerEntityManagerFactoryBean : Building JPA container EntityManagerFactory for persistence unit 'default'
2025-07-08 19:30:54.330  INFO 33688 --- [           main] o.hibernate.jpa.internal.util.LogHelper  : HHH000204: Processing PersistenceUnitInfo [
        name: default
        ...]
2025-07-08 19:30:54.414  INFO 33688 --- [           main] org.hibernate.Version                    : HHH000412: Hibernate Core {5.0.12.Final}
2025-07-08 19:30:54.416  INFO 33688 --- [           main] org.hibernate.cfg.Environment            : HHH000206: hibernate.properties not found
2025-07-08 19:30:54.418  INFO 33688 --- [           main] org.hibernate.cfg.Environment            : HHH000021: Bytecode provider name : javassist
2025-07-08 19:30:54.461  INFO 33688 --- [           main] o.hibernate.annotations.common.Version   : HCANN000001: Hibernate Commons Annotations {5.0.1.Final}
2025-07-08 19:30:54.575  INFO 33688 --- [           main] org.hibernate.dialect.Dialect            : HHH000400: Using dialect: org.hibernate.dialect.H2Dialect
2025-07-08 19:30:54.947  INFO 33688 --- [           main] org.hibernate.tool.hbm2ddl.SchemaExport  : HHH000227: Running hbm2ddl schema export
Hibernate: drop table user if exists
Hibernate: create table user (id bigint generated by default as identity, fname varchar(255), lname varchar(255), primary key (id))
2025-07-08 19:30:54.965  INFO 33688 --- [           main] org.hibernate.tool.hbm2ddl.SchemaExport  : HHH000230: Schema export complete
2025-07-08 19:30:54.990  INFO 33688 --- [           main] j.LocalContainerEntityManagerFactoryBean : Initialized JPA EntityManagerFactory for persistence unit 'default'
2025-07-08 19:30:55.379  INFO 33688 --- [           main] s.w.s.m.m.a.RequestMappingHandlerAdapter : Looking for @ControllerAdvice: org.springframework.boot.context.embedded.AnnotationConfigEmbeddedWebApplicationContext@5adef0ac: startup date [Tue Jul 08 19:30:52 EDT 2025]; root of context hierarchy
2025-07-08 19:30:55.447  INFO 33688 --- [           main] s.w.s.m.m.a.RequestMappingHandlerMapping : Mapped "{[/api/user/{id}],methods=[GET]}" onto public example.entities.User example.controllers.UserController.getUser(java.lang.Long)
2025-07-08 19:30:55.449  INFO 33688 --- [           main] s.w.s.m.m.a.RequestMappingHandlerMapping : Mapped "{[/api/user],methods=[POST]}" onto public example.entities.User example.controllers.UserController.createUser(example.entities.User)
2025-07-08 19:30:55.451  INFO 33688 --- [           main] s.w.s.m.m.a.RequestMappingHandlerMapping : Mapped "{[/api/user],methods=[PUT]}" onto public example.entities.User example.controllers.UserController.updateUser(example.entities.User)
2025-07-08 19:30:55.452  INFO 33688 --- [           main] s.w.s.m.m.a.RequestMappingHandlerMapping : Mapped "{[/api/users],methods=[GET]}" onto public java.util.List<example.entities.User> example.controllers.UserController.getUsers()
2025-07-08 19:30:55.452  INFO 33688 --- [           main] s.w.s.m.m.a.RequestMappingHandlerMapping : Mapped "{[/api/user/{id}],methods=[DELETE]}" onto public boolean example.controllers.UserController.deleteUser(java.lang.Long)
2025-07-08 19:30:55.453  INFO 33688 --- [           main] s.w.s.m.m.a.RequestMappingHandlerMapping : Mapped "{[/error]}" onto public org.springframework.http.ResponseEntity<java.util.Map<java.lang.String, java.lang.Object>> org.springframework.boot.autoconfigure.web.BasicErrorController.error(javax.servlet.http.HttpServletRequest)
2025-07-08 19:30:55.454  INFO 33688 --- [           main] s.w.s.m.m.a.RequestMappingHandlerMapping : Mapped "{[/error],produces=[text/html]}" onto public org.springframework.web.servlet.ModelAndView org.springframework.boot.autoconfigure.web.BasicErrorController.errorHtml(javax.servlet.http.HttpServletRequest,javax.servlet.http.HttpServletResponse)
2025-07-08 19:30:55.477  INFO 33688 --- [           main] o.s.w.s.handler.SimpleUrlHandlerMapping  : Mapped URL path [/webjars/**] onto handler of type [class org.springframework.web.servlet.resource.ResourceHttpRequestHandler]
2025-07-08 19:30:55.477  INFO 33688 --- [           main] o.s.w.s.handler.SimpleUrlHandlerMapping  : Mapped URL path [/**] onto handler of type [class org.springframework.web.servlet.resource.ResourceHttpRequestHandler]
2025-07-08 19:30:55.508  INFO 33688 --- [           main] o.s.w.s.handler.SimpleUrlHandlerMapping  : Mapped URL path [/**/favicon.ico] onto handler of type [class org.springframework.web.servlet.resource.ResourceHttpRequestHandler]
2025-07-08 19:30:55.529  INFO 33688 --- [           main] oConfiguration$WelcomePageHandlerMapping : Adding welcome page: class path resource [static/index.html]
2025-07-08 19:30:55.672  INFO 33688 --- [           main] o.s.j.e.a.AnnotationMBeanExporter        : Registering beans for JMX exposure on startup
2025-07-08 19:30:55.734  INFO 33688 --- [           main] s.b.c.e.t.TomcatEmbeddedServletContainer : Tomcat started on port(s): 8084 (http)
Hibernate: insert into user (id, fname, lname) values (null, ?, ?)
Hibernate: insert into user (id, fname, lname) values (null, ?, ?)
Hibernate: insert into user (id, fname, lname) values (null, ?, ?)
2025-07-08 19:30:55.793  INFO 33688 --- [           main] example.CrudBackendApplication           : Started CrudBackendApplication in 3.952 seconds (JVM running for 12.869)
2025-07-08 19:31:00.387  INFO 33688 --- [nio-8084-exec-1] o.a.c.c.C.[Tomcat].[localhost].[/]       : Initializing Spring FrameworkServlet 'dispatcherServlet'
2025-07-08 19:31:00.387  INFO 33688 --- [nio-8084-exec-1] o.s.web.servlet.DispatcherServlet        : FrameworkServlet 'dispatcherServlet': initialization started
2025-07-08 19:31:00.401  INFO 33688 --- [nio-8084-exec-1] o.s.web.servlet.DispatcherServlet        : FrameworkServlet 'dispatcherServlet': initialization completed in 14 ms
2025-07-08 19:31:00.584 ERROR 33688 --- [nio-8084-exec-7] o.a.c.c.C.[.[.[/].[dispatcherServlet]    : Servlet.service() for servlet [dispatcherServlet] in context with path [] threw exception [Handler dispatch failed; nested exception is java.lang.NoClassDefFoundError: com/sun/activation/registries/LogSupport] with root cause

java.lang.ClassNotFoundException: com.sun.activation.registries.LogSupport
        at java.base/java.net.URLClassLoader.findClass(URLClassLoader.java:471) ~[na:na]
        at java.base/java.lang.ClassLoader.loadClass(ClassLoader.java:589) ~[na:na]
        at java.base/java.lang.ClassLoader.loadClass(ClassLoader.java:522) ~[na:na]
        at javax.activation.MimetypesFileTypeMap.<init>(MimetypesFileTypeMap.java:100) ~[jakarta.activation-api-1.2.1.jar:1.2.1]
        at javax.activation.FileTypeMap.getDefaultFileTypeMap(FileTypeMap.java:110) ~[jakarta.activation-api-1.2.1.jar:1.2.1]
        at org.springframework.web.accept.PathExtensionContentNegotiationStrategy$ActivationMediaTypeFactory.initFileTypeMap(PathExtensionContentNegotiationStrategy.java:210) ~[spring-web-4.3.9.RELEASE.jar:4.3.9.RELEASE]
        at org.springframework.web.accept.PathExtensionContentNegotiationStrategy$ActivationMediaTypeFactory.<clinit>(PathExtensionContentNegotiationStrategy.java:176) ~[spring-web-4.3.9.RELEASE.jar:4.3.9.RELEASE]
        at org.springframework.web.accept.PathExtensionContentNegotiationStrategy.handleNoMatch(PathExtensionContentNegotiationStrategy.java:130) ~[spring-web-4.3.9.RELEASE.jar:4.3.9.RELEASE]
        at org.springframework.web.accept.ServletPathExtensionContentNegotiationStrategy.handleNoMatch(ServletPathExtensionContentNegotiationStrategy.java:78) ~[spring-web-4.3.9.RELEASE.jar:4.3.9.RELEASE]
        at org.springframework.web.accept.AbstractMappingContentNegotiationStrategy.resolveMediaTypeKey(AbstractMappingContentNegotiationStrategy.java:78) ~[spring-web-4.3.9.RELEASE.jar:4.3.9.RELEASE]
        at org.springframework.web.accept.AbstractMappingContentNegotiationStrategy.resolveMediaTypes(AbstractMappingContentNegotiationStrategy.java:61) ~[spring-web-4.3.9.RELEASE.jar:4.3.9.RELEASE]
        at org.springframework.boot.autoconfigure.web.WebMvcAutoConfiguration$OptionalPathExtensionContentNegotiationStrategy.resolveMediaTypes(WebMvcAutoConfiguration.java:600) ~[spring-boot-autoconfigure-1.5.4.RELEASE.jar:1.5.4.RELEASE]
        at org.springframework.web.accept.ContentNegotiationManager.resolveMediaTypes(ContentNegotiationManager.java:123) ~[spring-web-4.3.9.RELEASE.jar:4.3.9.RELEASE]
        at org.springframework.web.servlet.mvc.condition.ProducesRequestCondition.getAcceptedMediaTypes(ProducesRequestCondition.java:261) ~[spring-webmvc-4.3.9.RELEASE.jar:4.3.9.RELEASE]
        at org.springframework.web.servlet.mvc.condition.ProducesRequestCondition.getMatchingCondition(ProducesRequestCondition.java:194) ~[spring-webmvc-4.3.9.RELEASE.jar:4.3.9.RELEASE]
        at org.springframework.web.servlet.mvc.method.RequestMappingInfo.getMatchingCondition(RequestMappingInfo.java:214) ~[spring-webmvc-4.3.9.RELEASE.jar:4.3.9.RELEASE]
        at org.springframework.web.servlet.mvc.method.RequestMappingInfoHandlerMapping.getMatchingMapping(RequestMappingInfoHandlerMapping.java:94) ~[spring-webmvc-4.3.9.RELEASE.jar:4.3.9.RELEASE]
        at org.springframework.web.servlet.mvc.method.RequestMappingInfoHandlerMapping.getMatchingMapping(RequestMappingInfoHandlerMapping.java:58) ~[spring-webmvc-4.3.9.RELEASE.jar:4.3.9.RELEASE]
        at org.springframework.web.servlet.handler.AbstractHandlerMethodMapping.addMatchingMappings(AbstractHandlerMethodMapping.java:380) ~[spring-webmvc-4.3.9.RELEASE.jar:4.3.9.RELEASE]
        at org.springframework.web.servlet.handler.AbstractHandlerMethodMapping.lookupHandlerMethod(AbstractHandlerMethodMapping.java:347) ~[spring-webmvc-4.3.9.RELEASE.jar:4.3.9.RELEASE]
        at org.springframework.web.servlet.handler.AbstractHandlerMethodMapping.getHandlerInternal(AbstractHandlerMethodMapping.java:314) ~[spring-webmvc-4.3.9.RELEASE.jar:4.3.9.RELEASE]
        at org.springframework.web.servlet.handler.AbstractHandlerMethodMapping.getHandlerInternal(AbstractHandlerMethodMapping.java:61) ~[spring-webmvc-4.3.9.RELEASE.jar:4.3.9.RELEASE]
        at org.springframework.web.servlet.handler.AbstractHandlerMapping.getHandler(AbstractHandlerMapping.java:352) ~[spring-webmvc-4.3.9.RELEASE.jar:4.3.9.RELEASE]
        at org.springframework.web.servlet.DispatcherServlet.getHandler(DispatcherServlet.java:1160) ~[spring-webmvc-4.3.9.RELEASE.jar:4.3.9.RELEASE]
        at org.springframework.web.servlet.DispatcherServlet.doDispatch(DispatcherServlet.java:940) ~[spring-webmvc-4.3.9.RELEASE.jar:4.3.9.RELEASE]
        at org.springframework.web.servlet.DispatcherServlet.doService(DispatcherServlet.java:901) ~[spring-webmvc-4.3.9.RELEASE.jar:4.3.9.RELEASE]
        at org.springframework.web.servlet.FrameworkServlet.processRequest(FrameworkServlet.java:970) ~[spring-webmvc-4.3.9.RELEASE.jar:4.3.9.RELEASE]
        at org.springframework.web.servlet.FrameworkServlet.doGet(FrameworkServlet.java:861) ~[spring-webmvc-4.3.9.RELEASE.jar:4.3.9.RELEASE]
        at javax.servlet.http.HttpServlet.service(HttpServlet.java:635) ~[tomcat-embed-core-8.5.15.jar:8.5.15]
        at org.springframework.web.servlet.FrameworkServlet.service(FrameworkServlet.java:846) ~[spring-webmvc-4.3.9.RELEASE.jar:4.3.9.RELEASE]
        at javax.servlet.http.HttpServlet.service(HttpServlet.java:742) ~[tomcat-embed-core-8.5.15.jar:8.5.15]
        at org.apache.catalina.core.ApplicationFilterChain.internalDoFilter(ApplicationFilterChain.java:231) ~[tomcat-embed-core-8.5.15.jar:8.5.15]
        at org.apache.catalina.core.ApplicationFilterChain.doFilter(ApplicationFilterChain.java:166) ~[tomcat-embed-core-8.5.15.jar:8.5.15]
        at org.apache.tomcat.websocket.server.WsFilter.doFilter(WsFilter.java:52) ~[tomcat-embed-websocket-8.5.15.jar:8.5.15]
        at org.apache.catalina.core.ApplicationFilterChain.internalDoFilter(ApplicationFilterChain.java:193) ~[tomcat-embed-core-8.5.15.jar:8.5.15]
        at org.apache.catalina.core.ApplicationFilterChain.doFilter(ApplicationFilterChain.java:166) ~[tomcat-embed-core-8.5.15.jar:8.5.15]
        at org.springframework.web.filter.RequestContextFilter.doFilterInternal(RequestContextFilter.java:99) ~[spring-web-4.3.9.RELEASE.jar:4.3.9.RELEASE]
        at org.springframework.web.filter.OncePerRequestFilter.doFilter(OncePerRequestFilter.java:107) ~[spring-web-4.3.9.RELEASE.jar:4.3.9.RELEASE]
        at org.apache.catalina.core.ApplicationFilterChain.internalDoFilter(ApplicationFilterChain.java:193) ~[tomcat-embed-core-8.5.15.jar:8.5.15]
        at org.apache.catalina.core.ApplicationFilterChain.doFilter(ApplicationFilterChain.java:166) ~[tomcat-embed-core-8.5.15.jar:8.5.15]
        at org.springframework.web.filter.HttpPutFormContentFilter.doFilterInternal(HttpPutFormContentFilter.java:105) ~[spring-web-4.3.9.RELEASE.jar:4.3.9.RELEASE]
        at org.springframework.web.filter.OncePerRequestFilter.doFilter(OncePerRequestFilter.java:107) ~[spring-web-4.3.9.RELEASE.jar:4.3.9.RELEASE]
        at org.apache.catalina.core.ApplicationFilterChain.internalDoFilter(ApplicationFilterChain.java:193) ~[tomcat-embed-core-8.5.15.jar:8.5.15]
        at org.apache.catalina.core.ApplicationFilterChain.doFilter(ApplicationFilterChain.java:166) ~[tomcat-embed-core-8.5.15.jar:8.5.15]
        at org.springframework.web.filter.HiddenHttpMethodFilter.doFilterInternal(HiddenHttpMethodFilter.java:81) ~[spring-web-4.3.9.RELEASE.jar:4.3.9.RELEASE]
        at org.springframework.web.filter.OncePerRequestFilter.doFilter(OncePerRequestFilter.java:107) ~[spring-web-4.3.9.RELEASE.jar:4.3.9.RELEASE]
        at org.apache.catalina.core.ApplicationFilterChain.internalDoFilter(ApplicationFilterChain.java:193) ~[tomcat-embed-core-8.5.15.jar:8.5.15]
        at org.apache.catalina.core.ApplicationFilterChain.doFilter(ApplicationFilterChain.java:166) ~[tomcat-embed-core-8.5.15.jar:8.5.15]
        at org.springframework.web.filter.CharacterEncodingFilter.doFilterInternal(CharacterEncodingFilter.java:197) ~[spring-web-4.3.9.RELEASE.jar:4.3.9.RELEASE]
        at org.springframework.web.filter.OncePerRequestFilter.doFilter(OncePerRequestFilter.java:107) ~[spring-web-4.3.9.RELEASE.jar:4.3.9.RELEASE]
        at org.apache.catalina.core.ApplicationFilterChain.internalDoFilter(ApplicationFilterChain.java:193) ~[tomcat-embed-core-8.5.15.jar:8.5.15]
        at org.apache.catalina.core.ApplicationFilterChain.doFilter(ApplicationFilterChain.java:166) ~[tomcat-embed-core-8.5.15.jar:8.5.15]
        at org.apache.catalina.core.StandardWrapperValve.invoke(StandardWrapperValve.java:198) ~[tomcat-embed-core-8.5.15.jar:8.5.15]
        at org.apache.catalina.core.StandardContextValve.invoke(StandardContextValve.java:96) ~[tomcat-embed-core-8.5.15.jar:8.5.15]
        at org.apache.catalina.authenticator.AuthenticatorBase.invoke(AuthenticatorBase.java:478) ~[tomcat-embed-core-8.5.15.jar:8.5.15]
        at org.apache.catalina.core.StandardHostValve.invoke(StandardHostValve.java:140) ~[tomcat-embed-core-8.5.15.jar:8.5.15]
        at org.apache.catalina.valves.ErrorReportValve.invoke(ErrorReportValve.java:80) ~[tomcat-embed-core-8.5.15.jar:8.5.15]
        at org.apache.catalina.core.StandardEngineValve.invoke(StandardEngineValve.java:87) ~[tomcat-embed-core-8.5.15.jar:8.5.15]
        at org.apache.catalina.connector.CoyoteAdapter.service(CoyoteAdapter.java:342) ~[tomcat-embed-core-8.5.15.jar:8.5.15]
        at org.apache.coyote.http11.Http11Processor.service(Http11Processor.java:799) ~[tomcat-embed-core-8.5.15.jar:8.5.15]
        at org.apache.coyote.AbstractProcessorLight.process(AbstractProcessorLight.java:66) ~[tomcat-embed-core-8.5.15.jar:8.5.15]
        at org.apache.coyote.AbstractProtocol$ConnectionHandler.process(AbstractProtocol.java:861) ~[tomcat-embed-core-8.5.15.jar:8.5.15]
        at org.apache.tomcat.util.net.NioEndpoint$SocketProcessor.doRun(NioEndpoint.java:1455) ~[tomcat-embed-core-8.5.15.jar:8.5.15]
        at org.apache.tomcat.util.net.SocketProcessorBase.run(SocketProcessorBase.java:49) ~[tomcat-embed-core-8.5.15.jar:8.5.15]
        at java.base/java.util.concurrent.ThreadPoolExecutor.runWorker(ThreadPoolExecutor.java:1128) ~[na:na]
        at java.base/java.util.concurrent.ThreadPoolExecutor$Worker.run(ThreadPoolExecutor.java:628) ~[na:na]
        at org.apache.tomcat.util.threads.TaskThread$WrappingRunnable.run(TaskThread.java:61) ~[tomcat-embed-core-8.5.15.jar:8.5.15]
        at java.base/java.lang.Thread.run(Thread.java:829) ~[na:na]


```
#### Environment Tweaks 

* to downgrade Java
```cmd
java -version
```
see how to  modify `init.cmd`:
```diff
--- init.cmd    2025-07-08 17:35:51.191249900 -0400
+++ /c/java/init.cmd    2025-07-08 20:17:03.576227500 -0400
@@ -21,10 +21,9 @@

 REM Currently default to JDK 11
 REM downgrade to 1.8
-goto :JDK_1.8
 if "%JAVA_VERSION%"=="" set JAVA_VERSION=11.0.12
 if "%JAVA_HOME%"=="" set JAVA_HOME=%TOOL_HOME%\jdk-%JAVA_VERSION%
-:JDK_1.8
+
 REM in the past supporting JDK up to 1.8
 if "%JAVA_VERSION%"=="" set JAVA_VERSION=1.8.0_202
 if "%JAVA_HOME%"=="" set JAVA_HOME=%TOOL_HOME%\jdk%JAVA_VERSION%

```

### See Also
  * [list](https://github.com/search?q=angular+2+language%3AJava&type=repositories&l=Java&s=updated&o=desc&p=35) of Java Angular 2 projects ordered by age, from 2023 or ealier 
  * https://www.baeldung.com/spring-boot-h2-database
  * https://www.baeldung.com/spring-boot-h2-database
  * [Running NPM Scripts through maven](https://gist.github.com/phillipgreenii/7c954e3c3911e5c32bd0)
  * https://stackoverflow.com/questions/61986567/frontend-maven-plugin-when-it-runs-npm-run-build-cant-find-package-json-file
  
### Author

[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
