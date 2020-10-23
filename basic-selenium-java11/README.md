Standalone app to probe running Selenium tests on Java 11 runtime - 
Not a testng  or jupiter suite on surefile to improve chances for success in first iteration
based on "
https://github.com/rabbittrix/Java11-selenium-WebDriver
### Usage
#### Three Step
* compile and package jar on JDK11 and build Docker image with JDK 11 and maven,in three step
```sh
MAVEN_IMAGE=alpine-java11-maven
docker build -t $MAVEN_IMAGE -f Dockerfile.$MAVEN_IMAGE .
```

```sh
docker build -t selenium_java11 -f Dockerfile.java11 .
docker run -it -t selenium_java11 sh
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
        at example.App.main(App.java:16)

```
