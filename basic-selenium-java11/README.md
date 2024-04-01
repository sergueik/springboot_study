### Info

This directory contains a basic Selenium compiled as standalone app to practice running Selenium tests on Java 11 runtime -
Note, there is no testng  or jupiter suite on surefile in here, to improve chances for success in first iteration

### Usage

* check the versions on Chromium and chromium driver on the host
```sh
sudo apt-get install chromium-browser chromium-chromedriver
```
the versions of the driver may vary with the OS
e.g. 

```text
Version 95.0.4638.69 (Official Build) Built on Ubuntu , running on Ubuntu 18.04 (64-bit)
```

additionally download chromedriver to `~/Downloads`
```sh
chromedriver -v
```
```sh
pushd ~/Downloads
wget -q https://chromedriver.storage.googleapis.com/index.html?path=$(chromedriver -v | awk '{print $2}')/chromedriver_linux64.zip
unzip -ou chromedriver_linux64.zip
popd
```
* run locally the test
```sh
rm /tmp/sample*
mvn test
```
Note: the file may not download fully:

```sh
/tmp/sample.pdf.crdownload
```
The test performs assertions that either fully or partially download file to be present, and one of the assertions will be failing.
* run on developer machine in  standalone app
```sh
mvn -Dmaven.test.skip=true  package
rm -f /tmp/sample.pdf*
java -jar target/example.java_selenium.jar
```
* observe the file to be created (NOTE: the pdf dowload code is unstable)

* compile and package jar on Centos JDK8 Docker container
```sh
DOCKER_IMAGE=centos-jdk8-chrome
docker build -t $DOCKER_IMAGE -f Dockerfile.$DOCKER_IMAGE .
docker run -it $DOCKER_IMAGE
```
will respond with
```sh
Starting ChromeDriver 86.0.4240.22 (398b0743353ff36fb1b82468f63a3a93b4e2e89e-refs/branch-heads/4240@{#378}) on port 32480
Only local connections are allowed.
Please see https://chromedriver.chromium.org/security-considerations for suggestions on keeping ChromeDriver safe.
[1603576973.127][SEVERE]: bind() ChrfoamielDerdi:v eCra nwnaost  satsasritgend  rseuqcuceesstsefdu laldyd.r
ess (99)
Oct 24, 2020 10:02:59 PM org.openqa.selenium.remote.ProtocolHandshake createSession
INFO: Detected dialect: W3C
Hi, Julio
```
* inspect the dowloaded files
```sh
ID=$(docker container ls -a | grep $DOCKER_IMAGE | cut -f 1 -d ' ')
docker start $ID; docker exec -it  $ID sh
```
followed by
```sh
find  / -iname '*pdf'
```
NOTE:

the following will not work
```sh
echo $ID | xargs -IX docker exec -it  X sh
```
```sh
the input device is not a TTY
```
* compile and package jar on Centos JDK11 Docker container (aio)
```sh
DOCKER_IMAGE=centos-jdk11-chrome
docker build -t $DOCKER_IMAGE -f Dockerfile.$DOCKER_IMAGE .
docker run -it $DOCKER_IMAGE
```

* compile and package jar locally and run on Apline JDK8 and chromium without maven
```sh
DOCKER_IMAGE=alpine-jre8-chromium
docker build -t $DOCKER_IMAGE -f Dockerfile.$DOCKER_IMAGE .
docker run -it $DOCKER_IMAGE
```
NOTE, the `chromium-driver` package installs binary `/usr/bin/chromedriver`.
When this changes, update the `CMD` argument in the `Dockerfile.alpine-jre8-chromium` accordingly

* verify the project has JDK-sensitive code
```sh
mvn clean -Dmaven.test.skip=true package
java -cp target/java_selenium-0.3.0-SNAPSHOT.jar:target/lib/* example.App
```
the exception that was observed with Selenium `4.0.0-alpha-7` cannot be handled
```text
Exception in thread "main" java.lang.NoSuchMethodError:
java.io.FileReader.<init>(Ljava/io/File;Ljava/nio/charset/Charset;)V
```
* compile and package jar on JDK11/Maven "builder" container and copy the jar into a searate Docker image with JDK 11 and chromium:


```sh
DOCKER_IMAGE=alpine-jdk11-maven
docker build -t $DOCKER_IMAGE -f Dockerfile.$DOCKER_IMAGE .
```


followed by container run
* NOTE: maping host repository volume into container will have no effect, since source code is about to be compiled during *build* phase:
```sh
DOCKER_IMAGE=alpine-jdk11-chromium
docker build -t $DOCKER_IMAGE -f Dockerfile.$DOCKER_IMAGE .
docker run -v $(pwd)/maven/.m2:/tmp/maven/.m2 -it $DOCKER_IMAGE
```
the correct way is to either use the [cache mount](https://docs.docker.com/build/guide/mounts/) or have an explicit run step building java app

the process above will log
```sh
WARNING: Unable to find an exact match for CDP version 86, so returning the closest version found: 84
Nov 20, 2020 2:57:34 AM org.openqa.selenium.devtools.CdpVersionFinder findNearestMatch
INFO: Found CDP implementation for version 86 of 84
PortProber findFreePort -> 9973
Hi, Julio
```
### NOTE:

* seeing with Chrome __123__

```text
-------------------------------------------------------
 T E S T S
-------------------------------------------------------
Running example.AppTest
SLF4J: Failed to load class "org.slf4j.impl.StaticLoggerBinder".
SLF4J: Defaulting to no-operation (NOP) logger implementation
SLF4J: See http://www.slf4j.org/codes.html#StaticLoggerBinder for further details.
Starting ChromeDriver 123.0.6312.86 (9b72c47a053648d405376c5cf07999ed626728da-refs/branch-heads/6312@{#698}) on port 35626
Only local connections are allowed.
Please see https://chromedriver.chromium.org/security-co[1711677528.123][SEVERE]: bind() failed: Cannot assign requested address (99)
nsiderations for suggestions on keeping ChromeDriver safe.
ChromeDriver was started successfully.
Mar 29, 2024 2:59:28 AM org.openqa.selenium.remote.ProtocolHandshake createSession
INFO: Detected dialect: W3C
Mar 29, 2024 2:59:30 AM org.openqa.selenium.remote.http.WebSocket$Listener onError
WARNING: Invalid Status code=403 text=Forbidden
java.io.IOException: Invalid Status code=403 text=Forbidden
	at org.asynchttpclient.netty.handler.WebSocketHandler.abort(WebSocketHandler.java:92)
	at org.asynchttpclient.netty.handler.WebSocketHandler.handleRead(WebSocketHandler.java:118)
	at org.asynchttpclient.netty.handler.AsyncHttpClientHandler.channelRead(AsyncHttpClientHandler.java:78)
	at io.netty.channel.AbstractChannelHandlerContext.invokeChannelRead(AbstractChannelHandlerContext.java:379)
	at io.netty.channel.AbstractChannelHandlerContext.invokeChannelRead(AbstractChannelHandlerContext.java:365)
	at io.netty.channel.AbstractChannelHandlerContext.fireChannelRead(AbstractChannelHandlerContext.java:357)
	at io.netty.channel.CombinedChannelDuplexHandler$DelegatingChannelHandlerContext.fireChannelRead(CombinedChannelDuplexHandler.java:436)
	at io.netty.handler.codec.ByteToMessageDecoder.fireChannelRead(ByteToMessageDecoder.java:324)
	at io.netty.handler.codec.ByteToMessageDecoder.fireChannelRead(ByteToMessageDecoder.java:311)
	at io.netty.handler.codec.ByteToMessageDecoder.callDecode(ByteToMessageDecoder.java:432)
	at io.netty.handler.codec.ByteToMessageDecoder.channelRead(ByteToMessageDecoder.java:276)
	at io.netty.channel.CombinedChannelDuplexHandler.channelRead(CombinedChannelDuplexHandler.java:251)
	at io.netty.channel.AbstractChannelHandlerContext.invokeChannelRead(AbstractChannelHandlerContext.java:379)
	at io.netty.channel.AbstractChannelHandlerContext.invokeChannelRead(AbstractChannelHandlerContext.java:365)
	at io.netty.channel.AbstractChannelHandlerContext.fireChannelRead(AbstractChannelHandlerContext.java:357)
	at io.netty.channel.DefaultChannelPipeline$HeadContext.channelRead(DefaultChannelPipeline.java:1410)
	at io.netty.channel.AbstractChannelHandlerContext.invokeChannelRead(AbstractChannelHandlerContext.java:379)
	at io.netty.channel.AbstractChannelHandlerContext.invokeChannelRead(AbstractChannelHandlerContext.java:365)
	at io.netty.channel.DefaultChannelPipeline.fireChannelRead(DefaultChannelPipeline.java:919)
	at io.netty.channel.nio.AbstractNioByteChannel$NioByteUnsafe.read(AbstractNioByteChannel.java:166)
	at io.netty.channel.nio.NioEventLoop.processSelectedKey(NioEventLoop.java:719)
	at io.netty.channel.nio.NioEventLoop.processSelectedKeysOptimized(NioEventLoop.java:655)
	at io.netty.channel.nio.NioEventLoop.processSelectedKeys(NioEventLoop.java:581)
	at io.netty.channel.nio.NioEventLoop.run(NioEventLoop.java:493)
	at io.netty.util.concurrent.SingleThreadEventExecutor$4.run(SingleThreadEventExecutor.java:986)
	at io.netty.util.internal.ThreadExecutorMap$2.run(ThreadExecutorMap.java:74)
	at io.netty.util.concurrent.FastThreadLocalRunnable.run(FastThreadLocalRunnable.java:30)
	at java.base/java.lang.Thread.run(Thread.java:829)

Starting ChromeDriver 123.0.6312.86 (9b72c47a053648d405376c5cf07999ed626728da-refs/branch-heads/6312@{#698}) on port 34865
Only local connections are allowed.
Please see https://chromedriver.chromium.org/security-considerations for suggestions on keeping ChromeDriver safe.
[1711677570.519][SEVERE]: bind() failed: Cannot assign requested address (99)
ChromeDriver was started successfully.
Mar 29, 2024 2:59:35 AM org.openqa.selenium.remote.ProtocolHandshake createSession
INFO: Detected dialect: W3C
Mar 29, 2024 2:59:35 AM org.openqa.selenium.remote.http.WebSocket$Listener onError
WARNING: Invalid Status code=403 text=Forbidden
java.io.IOException: Invalid Status code=403 text=Forbidden
	at org.asynchttpclient.netty.handler.WebSocketHandler.abort(WebSocketHandler.java:92)
	at org.asynchttpclient.netty.handler.WebSocketHandler.handleRead(WebSocketHandler.java:118)
	at org.asynchttpclient.netty.handler.AsyncHttpClientHandler.channelRead(AsyncHttpClientHandler.java:78)
	at io.netty.channel.AbstractChannelHandlerContext.invokeChannelRead(AbstractChannelHandlerContext.java:379)
	at io.netty.channel.AbstractChannelHandlerContext.invokeChannelRead(AbstractChannelHandlerContext.java:365)
	at io.netty.channel.AbstractChannelHandlerContext.fireChannelRead(AbstractChannelHandlerContext.java:357)
	at io.netty.channel.CombinedChannelDuplexHandler$DelegatingChannelHandlerContext.fireChannelRead(CombinedChannelDuplexHandler.java:436)
	at io.netty.handler.codec.ByteToMessageDecoder.fireChannelRead(ByteToMessageDecoder.java:324)
	at io.netty.handler.codec.ByteToMessageDecoder.fireChannelRead(ByteToMessageDecoder.java:311)
	at io.netty.handler.codec.ByteToMessageDecoder.callDecode(ByteToMessageDecoder.java:432)
	at io.netty.handler.codec.ByteToMessageDecoder.channelRead(ByteToMessageDecoder.java:276)
	at io.netty.channel.CombinedChannelDuplexHandler.channelRead(CombinedChannelDuplexHandler.java:251)
	at io.netty.channel.AbstractChannelHandlerContext.invokeChannelRead(AbstractChannelHandlerContext.java:379)
	at io.netty.channel.AbstractChannelHandlerContext.invokeChannelRead(AbstractChannelHandlerContext.java:365)
	at io.netty.channel.AbstractChannelHandlerContext.fireChannelRead(AbstractChannelHandlerContext.java:357)
	at io.netty.channel.DefaultChannelPipeline$HeadContext.channelRead(DefaultChannelPipeline.java:1410)
	at io.netty.channel.AbstractChannelHandlerContext.invokeChannelRead(AbstractChannelHandlerContext.java:379)
	at io.netty.channel.AbstractChannelHandlerContext.invokeChannelRead(AbstractChannelHandlerContext.java:365)
	at io.netty.channel.DefaultChannelPipeline.fireChannelRead(DefaultChannelPipeline.java:919)
	at io.netty.channel.nio.AbstractNioByteChannel$NioByteUnsafe.read(AbstractNioByteChannel.java:166)
	at io.netty.channel.nio.NioEventLoop.processSelectedKey(NioEventLoop.java:719)
	at io.netty.channel.nio.NioEventLoop.processSelectedKeysOptimized(NioEventLoop.java:655)
	at io.netty.channel.nio.NioEventLoop.processSelectedKeys(NioEventLoop.java:581)
	at io.netty.channel.nio.NioEventLoop.run(NioEventLoop.java:493)
	at io.netty.util.concurrent.SingleThreadEventExecutor$4.run(SingleThreadEventExecutor.java:986)
	at io.netty.util.internal.ThreadExecutorMap$2.run(ThreadExecutorMap.java:74)
	at io.netty.util.concurrent.FastThreadLocalRunnable.run(FastThreadLocalRunnable.java:30)
	at java.base/java.lang.Thread.run(Thread.java:829)

Tests run: 2, Failures: 0, Errors: 2, Skipped: 0, Time elapsed: 49.974 sec <<< FAILURE!
downloadPDF(example.AppTest)  Time elapsed: 45.065 sec  <<< ERROR!
org.openqa.selenium.remote.http.ConnectionFailedException: Unable to establish websocket connection to http://localhost:41433/devtools/browser/cfe240ad-ac1d-4fcb-9883-6abb7e4ed2fc
Build info: version: '4.1.1', revision: 'e8fcc2cecf'
System info: host: 'sergueik71', ip: '127.0.1.1', os.name: 'Linux', os.arch: 'amd64', os.version: '5.4.0-42-generic', java.version: '11.0.19'
Driver info: driver.version: ChromeDriver
	at org.openqa.selenium.remote.http.netty.NettyWebSocket.<init>(NettyWebSocket.java:104)
	at org.openqa.selenium.remote.http.netty.NettyWebSocket.lambda$create$3(NettyWebSocket.java:137)
	at org.openqa.selenium.remote.http.netty.NettyClient.openSocket(NettyClient.java:118)
	at org.openqa.selenium.devtools.Connection.<init>(Connection.java:77)
	at org.openqa.selenium.chromium.ChromiumDriver.lambda$new$2(ChromiumDriver.java:124)
	at java.base/java.util.Optional.map(Optional.java:265)
	at org.openqa.selenium.chromium.ChromiumDriver.<init>(ChromiumDriver.java:122)
	at org.openqa.selenium.chrome.ChromeDriver.<init>(ChromeDriver.java:104)
	at org.openqa.selenium.chrome.ChromeDriver.<init>(ChromeDriver.java:91)
	at org.openqa.selenium.chrome.ChromeDriver.<init>(ChromeDriver.java:80)
	at example.AppTest.setUp(AppTest.java:55)
	at java.base/jdk.internal.reflect.NativeMethodAccessorImpl.invoke0(Native Method)
	at java.base/jdk.internal.reflect.NativeMethodAccessorImpl.invoke(NativeMethodAccessorImpl.java:62)
	at java.base/jdk.internal.reflect.DelegatingMethodAccessorImpl.invoke(DelegatingMethodAccessorImpl.java:43)
	at java.base/java.lang.reflect.Method.invoke(Method.java:566)
	at org.junit.runners.model.FrameworkMethod$1.runReflectiveCall(FrameworkMethod.java:59)
	at org.junit.internal.runners.model.ReflectiveCallable.run(ReflectiveCallable.java:12)
	at org.junit.runners.model.FrameworkMethod.invokeExplosively(FrameworkMethod.java:56)
	at org.junit.internal.runners.statements.RunBefores.invokeMethod(RunBefores.java:33)
	at org.junit.internal.runners.statements.RunBefores.evaluate(RunBefores.java:24)
	at org.junit.internal.runners.statements.RunAfters.evaluate(RunAfters.java:27)
	at org.junit.runners.ParentRunner$3.evaluate(ParentRunner.java:306)
	at org.junit.runners.BlockJUnit4ClassRunner$1.evaluate(BlockJUnit4ClassRunner.java:100)
	at org.junit.runners.ParentRunner.runLeaf(ParentRunner.java:366)
	at org.junit.runners.BlockJUnit4ClassRunner.runChild(BlockJUnit4ClassRunner.java:103)
	at org.junit.runners.BlockJUnit4ClassRunner.runChild(BlockJUnit4ClassRunner.java:63)
	at org.junit.runners.ParentRunner$4.run(ParentRunner.java:331)
	at org.junit.runners.ParentRunner$1.schedule(ParentRunner.java:79)
	at org.junit.runners.ParentRunner.runChildren(ParentRunner.java:329)
	at org.junit.runners.ParentRunner.access$100(ParentRunner.java:66)
	at org.junit.runners.ParentRunner$2.evaluate(ParentRunner.java:293)
	at org.junit.runners.ParentRunner$3.evaluate(ParentRunner.java:306)
	at org.junit.runners.ParentRunner.run(ParentRunner.java:413)
	at org.apache.maven.surefire.junit4.JUnit4Provider.execute(JUnit4Provider.java:252)
	at org.apache.maven.surefire.junit4.JUnit4Provider.executeTestSet(JUnit4Provider.java:141)
	at org.apache.maven.surefire.junit4.JUnit4Provider.invoke(JUnit4Provider.java:112)
	at java.base/jdk.internal.reflect.NativeMethodAccessorImpl.invoke0(Native Method)
	at java.base/jdk.internal.reflect.NativeMethodAccessorImpl.invoke(NativeMethodAccessorImpl.java:62)
	at java.base/jdk.internal.reflect.DelegatingMethodAccessorImpl.invoke(DelegatingMethodAccessorImpl.java:43)
	at java.base/java.lang.reflect.Method.invoke(Method.java:566)
	at org.apache.maven.surefire.util.ReflectionUtils.invokeMethodWithArray(ReflectionUtils.java:189)
	at org.apache.maven.surefire.booter.ProviderFactory$ProviderProxy.invoke(ProviderFactory.java:165)
	at org.apache.maven.surefire.booter.ProviderFactory.invokeProvider(ProviderFactory.java:85)
	at org.apache.maven.surefire.booter.ForkedBooter.runSuitesInProcess(ForkedBooter.java:115)
	at org.apache.maven.surefire.booter.ForkedBooter.main(ForkedBooter.java:75)

testAddInformationOfUser(example.AppTest)  Time elapsed: 4.66 sec  <<< ERROR!
org.openqa.selenium.remote.http.ConnectionFailedException: Unable to establish websocket connection to http://localhost:39253/devtools/browser/af681bad-209e-46bc-811a-9611220a959e
Build info: version: '4.1.1', revision: 'e8fcc2cecf'
System info: host: 'sergueik71', ip: '127.0.1.1', os.name: 'Linux', os.arch: 'amd64', os.version: '5.4.0-42-generic', java.version: '11.0.19'
Driver info: driver.version: ChromeDriver
	at org.openqa.selenium.remote.http.netty.NettyWebSocket.<init>(NettyWebSocket.java:104)
	at org.openqa.selenium.remote.http.netty.NettyWebSocket.lambda$create$3(NettyWebSocket.java:137)
	at org.openqa.selenium.remote.http.netty.NettyClient.openSocket(NettyClient.java:118)
	at org.openqa.selenium.devtools.Connection.<init>(Connection.java:77)
	at org.openqa.selenium.chromium.ChromiumDriver.lambda$new$2(ChromiumDriver.java:124)
	at java.base/java.util.Optional.map(Optional.java:265)
	at org.openqa.selenium.chromium.ChromiumDriver.<init>(ChromiumDriver.java:122)
	at org.openqa.selenium.chrome.ChromeDriver.<init>(ChromeDriver.java:104)
	at org.openqa.selenium.chrome.ChromeDriver.<init>(ChromeDriver.java:91)
	at org.openqa.selenium.chrome.ChromeDriver.<init>(ChromeDriver.java:80)
	at example.AppTest.setUp(AppTest.java:55)
	at java.base/jdk.internal.reflect.NativeMethodAccessorImpl.invoke0(Native Method)
	at java.base/jdk.internal.reflect.NativeMethodAccessorImpl.invoke(NativeMethodAccessorImpl.java:62)
	at java.base/jdk.internal.reflect.DelegatingMethodAccessorImpl.invoke(DelegatingMethodAccessorImpl.java:43)
	at java.base/java.lang.reflect.Method.invoke(Method.java:566)
	at org.junit.runners.model.FrameworkMethod$1.runReflectiveCall(FrameworkMethod.java:59)
	at org.junit.internal.runners.model.ReflectiveCallable.run(ReflectiveCallable.java:12)
	at org.junit.runners.model.FrameworkMethod.invokeExplosively(FrameworkMethod.java:56)
	at org.junit.internal.runners.statements.RunBefores.invokeMethod(RunBefores.java:33)
	at org.junit.internal.runners.statements.RunBefores.evaluate(RunBefores.java:24)
	at org.junit.internal.runners.statements.RunAfters.evaluate(RunAfters.java:27)
	at org.junit.runners.ParentRunner$3.evaluate(ParentRunner.java:306)
	at org.junit.runners.BlockJUnit4ClassRunner$1.evaluate(BlockJUnit4ClassRunner.java:100)
	at org.junit.runners.ParentRunner.runLeaf(ParentRunner.java:366)
	at org.junit.runners.BlockJUnit4ClassRunner.runChild(BlockJUnit4ClassRunner.java:103)
	at org.junit.runners.BlockJUnit4ClassRunner.runChild(BlockJUnit4ClassRunner.java:63)
	at org.junit.runners.ParentRunner$4.run(ParentRunner.java:331)
	at org.junit.runners.ParentRunner$1.schedule(ParentRunner.java:79)
	at org.junit.runners.ParentRunner.runChildren(ParentRunner.java:329)
	at org.junit.runners.ParentRunner.access$100(ParentRunner.java:66)
	at org.junit.runners.ParentRunner$2.evaluate(ParentRunner.java:293)
	at org.junit.runners.ParentRunner$3.evaluate(ParentRunner.java:306)
	at org.junit.runners.ParentRunner.run(ParentRunner.java:413)
	at org.apache.maven.surefire.junit4.JUnit4Provider.execute(JUnit4Provider.java:252)
	at org.apache.maven.surefire.junit4.JUnit4Provider.executeTestSet(JUnit4Provider.java:141)
	at org.apache.maven.surefire.junit4.JUnit4Provider.invoke(JUnit4Provider.java:112)
	at java.base/jdk.internal.reflect.NativeMethodAccessorImpl.invoke0(Native Method)
	at java.base/jdk.internal.reflect.NativeMethodAccessorImpl.invoke(NativeMethodAccessorImpl.java:62)
	at java.base/jdk.internal.reflect.DelegatingMethodAccessorImpl.invoke(DelegatingMethodAccessorImpl.java:43)
	at java.base/java.lang.reflect.Method.invoke(Method.java:566)
	at org.apache.maven.surefire.util.ReflectionUtils.invokeMethodWithArray(ReflectionUtils.java:189)
	at org.apache.maven.surefire.booter.ProviderFactory$ProviderProxy.invoke(ProviderFactory.java:165)
	at org.apache.maven.surefire.booter.ProviderFactory.invokeProvider(ProviderFactory.java:85)
	at org.apache.maven.surefire.booter.ForkedBooter.runSuitesInProcess(ForkedBooter.java:115)
	at org.apache.maven.surefire.booter.ForkedBooter.main(ForkedBooter.java:75)

```
### See Also

  * https://www.cyberciti.biz/faq/10-alpine-linux-apk-command-examples/
  * [Caching Maven Dependencies with Docker](https://www.baeldung.com/ops/docker-cache-maven-dependencies) - NOTE: requires `buildkit` feature to be enabled
 * [optimizing builds with cache management](https://docs.docker.com/build/cache/)
  * [long stackoverflow discussion of cons and pros of caching](https://stackoverflow.com/questions/42208442/maven-docker-cache-dependencies)
  * [speed up Maven Docker builds with Cache and buildkit](https://containers.fan/posts/speed-up-maven-docker-builds-with-cache/)
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
