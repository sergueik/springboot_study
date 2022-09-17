### Info

This directory contains log4j2 example demo project configured to log ito the file which path is defined in the environment
The only way to achieve this turns out through System Properties Lookup:

### Usage



```cmd
mkdir %APPDATA%\Logs
```
```cmd
del /q %APPDATA%\Logs\*.*
```
```cmd
mvn clean -DCUSTOM=%APPDATA% spring-boot:run
```
in separate console trigger the application work:
```sh
for cnt in $(seq 0 1 3); do curl "http://localhost:8085/example?data='${cnt}+test'"; done
```
or
```cmd
for /L %. in (1 1 10) do c:\tools\curl.exe "http://localhost:8085/example?data=%."
```
during the execution on Windows the logs will show zero size:
```cmd
dir %APPDATA%\Logs\*.*
```
```text
 Directory of C:\Users\Serguei\AppData\Roaming\Logs

09/17/2022  06:19 PM    <DIR>    .
09/17/2022  06:19 PM    <DIR>    ..
09/17/2022  06:19 PM         184 App.1.log.gz
09/17/2022  06:19 PM     0 App.log
09/17/2022  06:18 PM     0 Common.log
```
but will be available for reading:
```cmd
type %APPDATA%\Logs\App.log
```
```text
INFO  2022-09-17 18:19:38 LogHelper.java [LogHelper] - [info] - raw data '1 test'
INFO  2022-09-17 18:19:38 LogHelper.java [LogHelper] - [info] - handler received: '1 test'
INFO  2022-09-17 18:19:38 LogHelper.java [LogHelper] - [info] - raw data '2 test'
INFO  2022-09-17 18:19:38 LogHelper.java [LogHelper] - [info] - handler received: '2 test'
INFO  2022-09-17 18:19:38 LogHelper.java [LogHelper] - [info] - raw data '3 test'
INFO  2022-09-17 18:19:38 LogHelper.java [LogHelper] - [info] - handler received: '3 test'
```
after this the non zero size will be reported:
```text
 Directory of C:\Users\Serguei\AppData\Roaming\Logs
09/17/2022  06:19 PM    <DIR>    .
09/17/2022  06:19 PM    <DIR>    ..
09/17/2022  06:19 PM         184 App.1.log.gz
09/17/2022  06:19 PM         525 App.log
09/17/2022  06:19 PM     0 Common.log
```
### Note

If there is a typo in `fileName` or `filePattern` attribute e.g.

```xml
<File name="RootFileLogger" fileName="$${sys:CUSTOM}/logs/Common.log" append="true">
```

(the second dollar sign is needed in apttern expressions) the application will fail with
```text
java.io.IOException: The filename, directory name, or volume label syntax is incorrect java.io.IOException: The filename, directory name, or volume label syntax is incorrect
```
and massive error stack from `log4j` and `spring`
```
2022-09-17 18:22:37,893 main ERROR Unable to invoke factory method in class org.apache.logging.log4j.core.appender.FileAppender for element File: java.lang.IllegalStateException: No factory method found for class org.apache.logging.log4j.core.appender.FileAppender java.lang.IllegalStateException: No factory method found for class org.apache.logging.log4j.core.appender.FileAppender
  at org.apache.logging.log4j.core.config.plugins.util.PluginBuilder.findFactoryMethod(PluginBuilder.java:236)
  at org.apache.logging.log4j.core.config.plugins.util.PluginBuilder.build(PluginBuilder.java:134)
  at org.apache.logging.log4j.core.config.AbstractConfiguration.createPluginObject(AbstractConfiguration.java:1120)
  at org.apache.logging.log4j.core.config.AbstractConfiguration.createConfiguration(AbstractConfiguration.java:1045)
  at org.apache.logging.log4j.core.config.AbstractConfiguration.createConfiguration(AbstractConfiguration.java:1037)
  at org.apache.logging.log4j.core.config.AbstractConfiguration.doConfigure(AbstractConfiguration.java:651)
  at org.apache.logging.log4j.core.config.AbstractConfiguration.initialize(AbstractConfiguration.java:247)
  at org.apache.logging.log4j.core.config.AbstractConfiguration.start(AbstractConfiguration.java:293)
  at org.apache.logging.log4j.core.LoggerContext.setConfiguration(LoggerContext.java:626)
  at org.apache.logging.log4j.core.LoggerContext.reconfigure(LoggerContext.java:699)
  at org.apache.logging.log4j.core.LoggerContext.reconfigure(LoggerContext.java:716)
  at org.apache.logging.log4j.core.LoggerContext.start(LoggerContext.java:270)
  at org.apache.logging.log4j.core.impl.Log4jContextFactory.getContext(Log4jContextFactory.java:155)
  at org.apache.logging.log4j.core.impl.Log4jContextFactory.getContext(Log4jContextFactory.java:47)
  at org.apache.logging.log4j.LogManager.getContext(LogManager.java:196)
  at org.apache.logging.log4j.LogManager.getLogger(LogManager.java:599)
  at example.LogHelper.<clinit>(LogHelper.java:11)
  at sun.reflect.NativeConstructorAccessorImpl.newInstance0(Native Method)

  at sun.reflect.NativeConstructorAccessorImpl.newInstance(NativeConstructorAccessorImpl.java:62)
  at sun.reflect.DelegatingConstructorAccessorImpl.newInstance(DelegatingConstructorAccessorImpl.java:45)
  at java.lang.reflect.Constructor.newInstance(Constructor.java:423)
  at org.springframework.beans.BeanUtils.instantiateClass(BeanUtils.java:142)
  at org.springframework.beans.factory.support.SimpleInstantiationStrategy.instantiate(SimpleInstantiationStrategy.java:89)  at org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory.instantiateBean(AbstractAutowireCapableBeanFactory.java:1147)
  at org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory.createBeanInstance(AbstractAutowireCapableBeanFactory.java:1099)
  at org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory.doCreateBean(AbstractAutowireCapableBeanFactory.java:513)
  at org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory.createBean(AbstractAutowireCapableBeanFactory.java:483)
  at org.springframework.beans.factory.support.AbstractBeanFactory$1.getObject(AbstractBeanFactory.java:306)
  at org.springframework.beans.factory.support.DefaultSingletonBeanRegistry.getSingleton(DefaultSingletonBeanRegistry.java:230)
  at org.springframework.beans.factory.support.AbstractBeanFactory.doGetBean(AbstractBeanFactory.java:302)
  at org.springframework.beans.factory.support.AbstractBeanFactory.getBean(AbstractBeanFactory.java:202)
  at org.springframework.beans.factory.config.DependencyDescriptor.resolveCandidate(DependencyDescriptor.java:208)
  at org.springframework.beans.factory.support.DefaultListableBeanFactory.doResolveDependency(DefaultListableBeanFactory.java:1138)
  at org.springframework.beans.factory.support.DefaultListableBeanFactory.resolveDependency(DefaultListableBeanFactory.java:1066)
  at org.springframework.beans.factory.annotation.AutowiredAnnotationBeanPostProcessor$AutowiredFieldElement.inject(AutowiredAnnotationBeanPostProcessor.java:585)
  at org.springframework.beans.factory.annotation.InjectionMetadata.inject(InjectionMetadata.java:88)
  at org.springframework.beans.factory.annotation.AutowiredAnnotationBeanPostProcessor.postProcessPropertyValues(AutowiredAnnotationBeanPostProcessor.java:366)
  at org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory.populateBean(AbstractAutowireCapableBeanFactory.java:1264)
  at org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory.doCreateBean(AbstractAutowireCapableBeanFactory.java:553)
  at org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory.createBean(AbstractAutowireCapableBeanFactory.java:483)
  at org.springframework.beans.factory.support.AbstractBeanFactory$1.getObject(AbstractBeanFactory.java:306)
  at org.springframework.beans.factory.support.DefaultSingletonBeanRegistry.getSingleton(DefaultSingletonBeanRegistry.java:230)
  at org.springframework.beans.factory.support.AbstractBeanFactory.doGetBean(AbstractBeanFactory.java:302)
  at org.springframework.beans.factory.support.AbstractBeanFactory.getBean(AbstractBeanFactory.java:197)
  at org.springframework.beans.factory.support.DefaultListableBeanFactory.preInstantiateSingletons(DefaultListableBeanFactory.java:761)
  at org.springframework.context.support.AbstractApplicationContext.finishBeanFactoryInitialization(AbstractApplicationContext.java:867)
  at org.springframework.context.support.AbstractApplicationContext.refresh(AbstractApplicationContext.java:543)
  at org.springframework.boot.context.embedded.EmbeddedWebApplicationContext.refresh(EmbeddedWebApplicationContext.java:122)
  at org.springframework.boot.SpringApplication.refresh(SpringApplication.java:693)
  at org.springframework.boot.SpringApplication.refreshContext(SpringApplication.java:360)
  at org.springframework.boot.SpringApplication.run(SpringApplication.java:303)
  at org.springframework.boot.SpringApplication.run(SpringApplication.java:1118)
  at org.springframework.boot.SpringApplication.run(SpringApplication.java:1107)
  at example.Example.main(Example.java:27)
  at sun.reflect.NativeMethodAccessorImpl.invoke0(Native Method)
  at sun.reflect.NativeMethodAccessorImpl.invoke(NativeMethodAccessorImpl.java:62)
  at sun.reflect.DelegatingMethodAccessorImpl.invoke(DelegatingMethodAccessorImpl.java:43)
  at java.lang.reflect.Method.invoke(Method.java:498)
  at org.springframework.boot.maven.AbstractRunMojo$LaunchRunner.run(AbstractRunMojo.java:527)
  at java.lang.Thread.run(Thread.java:745)
```
### See Also

  * [discussion](https://stackoverflow.com/questions/13066042/how-to-give-environmental-variable-path-for-file-appender-in-configuration-file)
  * __log4j__ [configuration](https://logging.apache.org/log4j/2.x/manual/configuration.html) 
and [lookup](https://logging.apache.org/log4j/2.x/manual/lookups.html) documentation - NOTE, not all recipes appear to work

### Author

[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
