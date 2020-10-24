### Info

Standalone app to probe running Selenium tests on Java 11 runtime - 
Not a testng  or jupiter suite on surefile to improve chances for success in first iteration

### Usage
* run local test
```sh
mvn -DchromeDriverPath=/usr/bin/chromedriver test package
```
* run on developer machine
```sh
java -jar target/example.java_selenium.jar
```
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
* compile and package jar on Centos JDK11 Docker container
```sh
DOCKER_IMAGE=centos-jdk11-chrome
docker build -t $DOCKER_IMAGE -f Dockerfile.$DOCKER_IMAGE .
docker run -it $DOCKER_IMAGE 
```

* compile and package jar locally and run on Apline JDK8 and chrome without maven
```sh
DOCKER_IMAGE=alpine-java8-chromium
docker build -t $DOCKER_IMAGE -f Dockerfile.$DOCKER_IMAGE .
docker run -it $DOCKER_IMAGE sh
```
NOTE, the `chromium-driver` package installs binary `/usr/bin/chromedriver`. When this changes, update the `CMD` argument accordingly
* compile and package jar on JDK11 and build Docker image with JDK 11 and maven,in three step
```sh
DOCKER_IMAGE=alpine-java11-maven
docker build -t $DOCKER_IMAGE -f Dockerfile.$DOCKER_IMAGE .
```

```sh
docker build -t alpine-java11-chromium -f Dockerfile.java11 .
docker run -it -t selenium_java11 sh
docker run -it $DOCKER_IMAGE
```

```sh
cd /application
java -jar app.jar
```

currently is failing with:
```sh
Starting ChromeDriver 85.0.4183.121 (a81aa729a8e1fd413943a339393c82e7b8055ddc-refs/branch-heads/4183@{#1864}) on port 16087
Only local connections are allowed.
Please see https://chromedriver.chromium.org/security-considerations for suggestions on keeping [1603486187.544][SEVERE]: bind() failed: Address not available (99)
ChromeDriver safe.
ChromeDriver was started successfully.
log4j:WARN No appenders could be found for logger (org.apache.http.client.protocol.RequestAddCookies).
log4j:WARN Please initialize the log4j system properly.
Exception in thread "main" org.openqa.selenium.WebDriverException: unknown error: Chrome failed to start: exited abnormally.
  (chrome not reachable)
  (The process started from chrome location /usr/lib/chromium/chrome is no longer running, so ChromeDriver is assuming that Chrome has crashed.)
  (Driver info: chromedriver=85.0.4183.121 (a81aa729a8e1fd413943a339393c82e7b8055ddc-refs/branch-heads/4183@{#1864}),platform=Linux 5.4.0-42-generic x86_64) (WARNING: The server did not provide any stacktrace information)
Command duration or timeout: 1.52 seconds
Build info: version: 'unknown', revision: 'unknown', time: 'unknown'
System info: host: '5a986964ff68', ip: '172.17.0.2', os.name: 'Linux', os.arch: 'amd64', os.version: '5.4.0-42-generic', java.version: '11.0.9'
Driver info: org.openqa.selenium.chrome.ChromeDriver
        at java.base/jdk.internal.reflect.NativeConstructorAccessorImpl.newInstance0(Native Method)
        at java.base/jdk.internal.reflect.NativeConstructorAccessorImpl.newInstance(NativeConstructorAccessorImpl.java:62)
        at java.base/jdk.internal.reflect.DelegatingConstructorAccessorImpl.newInstance(DelegatingConstructorAccessorImpl.java:45)
        at java.base/java.lang.reflect.Constructor.newInstance(Constructor.java:490)
        at org.openqa.selenium.remote.ErrorHandler.createThrowable(ErrorHandler.java:206)
        at org.openqa.selenium.remote.ErrorHandler.throwIfResponseFailed(ErrorHandler.java:158)
        at org.openqa.selenium.remote.RemoteWebDriver.execute(RemoteWebDriver.java:678)
        at org.openqa.selenium.remote.RemoteWebDriver.startSession(RemoteWebDriver.java:249)
        at org.openqa.selenium.remote.RemoteWebDriver.<init>(RemoteWebDriver.java:131)
        at org.openqa.selenium.remote.RemoteWebDriver.<init>(RemoteWebDriver.java:144)
        at org.openqa.selenium.chrome.ChromeDriver.<init>(ChromeDriver.java:170)
        at org.openqa.selenium.chrome.ChromeDriver.<init>(ChromeDriver.java:159)
        at org.openqa.selenium.chrome.ChromeDriver.<init>(ChromeDriver.java:148)
        at example.App.navigate(App.java:31)
        at example.App.main(App.java:17)

```
and running `gogogle-chrome` (`chromium`) alone shows the error
```sh
Failed to move to new namespace: PID namespaces supported, Network namespace supported, but failed: errno = Operation not permitted
```
check the options passed to __chromedriver__ to include the following:
```java
"--headless", "--window-size=1200x800", "--no-sandbox", "--remote-debugging-address=0.0.0.0", "--remote-debugging-port=9222", "--disable-gpu" 
```
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
