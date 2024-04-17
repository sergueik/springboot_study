### Info

Clone of [basic-tests](https://github.com/sergueik/springboot_study/tree/master/basic-tests) with `pom.xml` using `dependencyManagement` instead of `parent` to specify Spring Boot version. 

### NOTE

This is working only partially: the verison of maven plugin is not compatible  with Java 11, indirectly:
```sh
mvn clean spring-boot:run
```
fails with
```text

[INFO] Compiling 1 source file to /home/sergueik/src/springboot_study/basic-alt-pom-structure/target/test-classes
[WARNING] bootstrap class path not set in conjunction with -source 8
[INFO] 
[INFO] <<< spring-boot-maven-plugin:3.2.4:run (default-cli) < test-compile @ alt-pom-structure <<<
[INFO] 
[INFO] --- spring-boot-maven-plugin:3.2.4:run (default-cli) @ alt-pom-structure ---
[WARNING] Error injecting: org.springframework.boot.maven.RunMojo
java.lang.TypeNotPresentException: Type org.springframework.boot.maven.RunMojo not present
	at org.eclipse.sisu.space.URLClassSpace.loadClass(URLClassSpace.java:147)
	at org.eclipse.sisu.space.NamedClass.load(NamedClass.java:46)
	at org.eclipse.sisu.space.AbstractDeferredClass.get(AbstractDeferredClass.java:48)
	at com.google.inject.internal.ProviderInternalFactory.provision(ProviderInternalFactory.java:81)
	at com.google.inject.internal.InternalFactoryToInitializableAdapter.provision(InternalFactoryToInitializableAdapter.java:53)
	at com.google.inject.internal.ProviderInternalFactory$1.call(ProviderInternalFactory.java:65)
	at com.google.inject.internal.ProvisionListenerStackCallback$Provision.provision(ProvisionListenerStackCallback.java:115)
	at org.eclipse.sisu.bean.BeanScheduler$Activator.onProvision(BeanScheduler.java:176)
	at com.google.inject.internal.ProvisionListenerStackCallback$Provision.provision(ProvisionListenerStackCallback.java:126)
	at com.google.inject.internal.ProvisionListenerStackCallback.provision(ProvisionListenerStackCallback.java:68)
	at com.google.inject.internal.ProviderInternalFactory.circularGet(ProviderInternalFactory.java:63)
	at com.google.inject.internal.InternalFactoryToInitializableAdapter.get(InternalFactoryToInitializableAdapter.java:45)
	at com.google.inject.internal.InjectorImpl$2$1.call(InjectorImpl.java:1016)
	at com.google.inject.internal.InjectorImpl.callInContext(InjectorImpl.java:1092)
	at com.google.inject.internal.InjectorImpl$2.get(InjectorImpl.java:1012)
	at org.eclipse.sisu.inject.Guice4$1.get(Guice4.java:162)
	at org.eclipse.sisu.inject.LazyBeanEntry.getValue(LazyBeanEntry.java:81)
	at org.eclipse.sisu.plexus.LazyPlexusBean.getValue(LazyPlexusBean.java:51)
	at org.codehaus.plexus.DefaultPlexusContainer.lookup(DefaultPlexusContainer.java:263)
	at org.codehaus.plexus.DefaultPlexusContainer.lookup(DefaultPlexusContainer.java:255)
	at org.apache.maven.plugin.internal.DefaultMavenPluginManager.getConfiguredMojo(DefaultMavenPluginManager.java:517)
	at org.apache.maven.plugin.DefaultBuildPluginManager.executeMojo(DefaultBuildPluginManager.java:121)
	at org.apache.maven.lifecycle.internal.MojoExecutor.execute(MojoExecutor.java:207)
	at org.apache.maven.lifecycle.internal.MojoExecutor.execute(MojoExecutor.java:153)
	at org.apache.maven.lifecycle.internal.MojoExecutor.execute(MojoExecutor.java:145)
	at org.apache.maven.lifecycle.internal.LifecycleModuleBuilder.buildProject(LifecycleModuleBuilder.java:116)
	at org.apache.maven.lifecycle.internal.LifecycleModuleBuilder.buildProject(LifecycleModuleBuilder.java:80)
	at org.apache.maven.lifecycle.internal.builder.singlethreaded.SingleThreadedBuilder.build(SingleThreadedBuilder.java:51)
	at org.apache.maven.lifecycle.internal.LifecycleStarter.execute(LifecycleStarter.java:128)
	at org.apache.maven.DefaultMaven.doExecute(DefaultMaven.java:307)
	at org.apache.maven.DefaultMaven.doExecute(DefaultMaven.java:193)
	at org.apache.maven.DefaultMaven.execute(DefaultMaven.java:106)
	at org.apache.maven.cli.MavenCli.execute(MavenCli.java:863)
	at org.apache.maven.cli.MavenCli.doMain(MavenCli.java:288)
	at org.apache.maven.cli.MavenCli.main(MavenCli.java:199)
	at java.base/jdk.internal.reflect.NativeMethodAccessorImpl.invoke0(Native Method)
	at java.base/jdk.internal.reflect.NativeMethodAccessorImpl.invoke(NativeMethodAccessorImpl.java:62)
	at java.base/jdk.internal.reflect.DelegatingMethodAccessorImpl.invoke(DelegatingMethodAccessorImpl.java:43)
	at java.base/java.lang.reflect.Method.invoke(Method.java:566)
	at org.codehaus.plexus.classworlds.launcher.Launcher.launchEnhanced(Launcher.java:289)
	at org.codehaus.plexus.classworlds.launcher.Launcher.launch(Launcher.java:229)
	at org.codehaus.plexus.classworlds.launcher.Launcher.mainWithExitCode(Launcher.java:415)
	at org.codehaus.plexus.classworlds.launcher.Launcher.main(Launcher.java:356)
Caused by: java.lang.UnsupportedClassVersionError: org/springframework/boot/maven/RunMojo has been compiled by a more recent version of the Java Runtime (class file version 61.0), this version of the Java Runtime only recognizes class file versions up to 55.0
	at java.base/java.lang.ClassLoader.defineClass1(Native Method)
	at java.base/java.lang.ClassLoader.defineClass(ClassLoader.java:1017)
	at java.base/java.security.SecureClassLoader.defineClass(SecureClassLoader.java:174)
	at java.base/java.net.URLClassLoader.defineClass(URLClassLoader.java:555)
	at java.base/java.net.URLClassLoader$1.run(URLClassLoader.java:458)
	at java.base/java.net.URLClassLoader$1.run(URLClassLoader.java:452)
	at java.base/java.security.AccessController.doPrivileged(Native Method)
	at java.base/java.net.URLClassLoader.findClass(URLClassLoader.java:451)
	at org.codehaus.plexus.classworlds.realm.ClassRealm.loadClassFromSelf(ClassRealm.java:401)
	at org.codehaus.plexus.classworlds.strategy.SelfFirstStrategy.loadClass(SelfFirstStrategy.java:42)
	at org.codehaus.plexus.classworlds.realm.ClassRealm.unsynchronizedLoadClass(ClassRealm.java:271)
	at org.codehaus.plexus.classworlds.realm.ClassRealm.loadClass(ClassRealm.java:247)
	at org.codehaus.plexus.classworlds.realm.ClassRealm.loadClass(ClassRealm.java:239)
	at org.eclipse.sisu.space.URLClassSpace.loadClass(URLClassSpace.java:139)
	... 42 more
[INFO] ------------------------------------------------------------------------
[INFO] BUILD FAILURE
[INFO] ------------------------------------------------------------------------
[INFO] Total time: 6.262 s
[INFO] Finished at: 2024-04-16T19:32:39-04:00
[INFO] Final Memory: 27M/104M
[INFO] ------------------------------------------------------------------------
[ERROR] Failed to execute goal org.springframework.boot:spring-boot-maven-plugin:3.2.4:run (default-cli) on project alt-pom-structure: Execution default-cli of goal org.springframework.boot:spring-boot-maven-plugin:3.2.4:run failed: Unable to load the mojo 'run' in the plugin 'org.springframework.boot:spring-boot-maven-plugin:3.2.4' due to an API incompatibility: org.codehaus.plexus.component.repository.exception.ComponentLookupException: org/springframework/boot/maven/RunMojo has been compiled by a more recent version of the Java Runtime (class file version 61.0), this version of the Java Runtime only recognizes class file versions up to 55.0
[ERROR] -----------------------------------------------------
[ERROR] realm =    plugin>org.springframework.boot:spring-boot-maven-plugin:3.2.4
[ERROR] strategy = org.codehaus.plexus.classworlds.strategy.SelfFirstStrategy
[ERROR] urls[0] = file:/home/sergueik/.m2/repository/org/springframework/boot/spring-boot-maven-plugin/3.2.4/spring-boot-maven-plugin-3.2.4.jar
[ERROR] urls[1] = file:/home/sergueik/.m2/repository/org/springframework/boot/spring-boot-buildpack-platform/3.2.4/spring-boot-buildpack-platform-3.2.4.jar
[ERROR] urls[2] = file:/home/sergueik/.m2/repository/com/fasterxml/jackson/core/jackson-databind/2.14.2/jackson-databind-2.14.2.jar
[ERROR] urls[3] = file:/home/sergueik/.m2/repository/com/fasterxml/jackson/core/jackson-annotations/2.14.2/jackson-annotations-2.14.2.jar
[ERROR] urls[4] = file:/home/sergueik/.m2/repository/com/fasterxml/jackson/core/jackson-core/2.14.2/jackson-core-2.14.2.jar
[ERROR] urls[5] = file:/home/sergueik/.m2/repository/com/fasterxml/jackson/module/jackson-module-parameter-names/2.14.2/jackson-module-parameter-names-2.14.2.jar
[ERROR] urls[6] = file:/home/sergueik/.m2/repository/net/java/dev/jna/jna-platform/5.13.0/jna-platform-5.13.0.jar
[ERROR] urls[7] = file:/home/sergueik/.m2/repository/net/java/dev/jna/jna/5.13.0/jna-5.13.0.jar
[ERROR] urls[8] = file:/home/sergueik/.m2/repository/org/apache/commons/commons-compress/1.21/commons-compress-1.21.jar
[ERROR] urls[9] = file:/home/sergueik/.m2/repository/org/apache/httpcomponents/client5/httpclient5/5.2.3/httpclient5-5.2.3.jar
[ERROR] urls[10] = file:/home/sergueik/.m2/repository/org/apache/httpcomponents/core5/httpcore5/5.2.4/httpcore5-5.2.4.jar
[ERROR] urls[11] = file:/home/sergueik/.m2/repository/org/apache/httpcomponents/core5/httpcore5-h2/5.2.4/httpcore5-h2-5.2.4.jar
[ERROR] urls[12] = file:/home/sergueik/.m2/repository/org/tomlj/tomlj/1.0.0/tomlj-1.0.0.jar
[ERROR] urls[13] = file:/home/sergueik/.m2/repository/org/antlr/antlr4-runtime/4.7.2/antlr4-runtime-4.7.2.jar
[ERROR] urls[14] = file:/home/sergueik/.m2/repository/com/google/code/findbugs/jsr305/3.0.2/jsr305-3.0.2.jar
[ERROR] urls[15] = file:/home/sergueik/.m2/repository/org/springframework/boot/spring-boot-loader-tools/3.2.4/spring-boot-loader-tools-3.2.4.jar
[ERROR] urls[16] = file:/home/sergueik/.m2/repository/org/apache/maven/shared/maven-common-artifact-filters/3.3.2/maven-common-artifact-filters-3.3.2.jar
[ERROR] urls[17] = file:/home/sergueik/.m2/repository/org/springframework/spring-core/6.1.5/spring-core-6.1.5.jar
[ERROR] urls[18] = file:/home/sergueik/.m2/repository/org/springframework/spring-jcl/6.1.5/spring-jcl-6.1.5.jar
[ERROR] urls[19] = file:/home/sergueik/.m2/repository/org/springframework/spring-context/6.1.5/spring-context-6.1.5.jar
[ERROR] urls[20] = file:/home/sergueik/.m2/repository/org/springframework/spring-aop/6.1.5/spring-aop-6.1.5.jar
[ERROR] urls[21] = file:/home/sergueik/.m2/repository/org/springframework/spring-beans/6.1.5/spring-beans-6.1.5.jar
[ERROR] urls[22] = file:/home/sergueik/.m2/repository/org/springframework/spring-expression/6.1.5/spring-expression-6.1.5.jar
[ERROR] urls[23] = file:/home/sergueik/.m2/repository/io/micrometer/micrometer-observation/1.12.4/micrometer-observation-1.12.4.jar
[ERROR] urls[24] = file:/home/sergueik/.m2/repository/io/micrometer/micrometer-commons/1.12.4/micrometer-commons-1.12.4.jar
[ERROR] urls[25] = file:/home/sergueik/.m2/repository/org/sonatype/plexus/plexus-build-api/0.0.7/plexus-build-api-0.0.7.jar
[ERROR] urls[26] = file:/home/sergueik/.m2/repository/org/codehaus/plexus/plexus-utils/1.5.8/plexus-utils-1.5.8.jar
[ERROR] urls[27] = file:/home/sergueik/.m2/repository/org/apache/maven/plugins/maven-shade-plugin/3.5.0/maven-shade-plugin-3.5.0.jar
[ERROR] urls[28] = file:/home/sergueik/.m2/repository/org/ow2/asm/asm/9.5/asm-9.5.jar
[ERROR] urls[29] = file:/home/sergueik/.m2/repository/org/ow2/asm/asm-commons/9.5/asm-commons-9.5.jar
[ERROR] urls[30] = file:/home/sergueik/.m2/repository/org/ow2/asm/asm-tree/9.5/asm-tree-9.5.jar
[ERROR] urls[31] = file:/home/sergueik/.m2/repository/org/jdom/jdom2/2.0.6.1/jdom2-2.0.6.1.jar
[ERROR] urls[32] = file:/home/sergueik/.m2/repository/org/apache/maven/shared/maven-dependency-tree/3.2.1/maven-dependency-tree-3.2.1.jar
[ERROR] urls[33] = file:/home/sergueik/.m2/repository/org/eclipse/aether/aether-util/1.0.0.v20140518/aether-util-1.0.0.v20140518.jar
[ERROR] urls[34] = file:/home/sergueik/.m2/repository/commons-io/commons-io/2.13.0/commons-io-2.13.0.jar
[ERROR] urls[35] = file:/home/sergueik/.m2/repository/org/vafer/jdependency/2.8.0/jdependency-2.8.0.jar
[ERROR] urls[36] = file:/home/sergueik/.m2/repository/org/apache/commons/commons-collections4/4.4/commons-collections4-4.4.jar
[ERROR] Number of foreign imports: 1
[ERROR] import: Entry[import  from realm ClassRealm[maven.api, parent: null]]
[ERROR] 
[ERROR] -----------------------------------------------------
[ERROR] -> [Help 1]
[ERROR] 
[ERROR] To see the full stack trace of the errors, re-run Maven with the -e switch.
[ERROR] Re-run Maven using the -X switch to enable full debug logging.
[ERROR] 
[ERROR] For more information about the errors and possible solutions, please read the following articles:
[ERROR] [Help 1] http://cwiki.apache.org/confluence/display/MAVEN/PluginContainerException

```

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
