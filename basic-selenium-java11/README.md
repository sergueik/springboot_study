### Info

This directory contains a basi Selenium compiled as standalone app to practice running Selenium tests on Java 11 runtime -
Note, there is no testng  or jupiter suite on surefile in here, to improve chances for success in first iteration

### Usage
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
NOTE, the `chromium-driver` package installs binary `/usr/bin/chromedriver`. When this changes, update the `CMD` argument accordingly

* verify the project has JDK-sensitive code
```sh
mvn clean -Dmaven.test.skip=true package
java -cp target/java_selenium-0.3.0-SNAPSHOT.jar:target/lib/* example.App
```
the exception cannot be handled
```sh
Exception in thread "main" java.lang.NoSuchMethodError: java.io.FileReader.<init>(Ljava/io/File;Ljava/nio/charset/Charset;)V
```
* compile and package jar on JDK11/Maven "builder" container and copy the jar into a searate Docker image with JDK 11 and chromium:


```sh
DOCKER_IMAGE=alpine-jdk11-maven
docker build -t $DOCKER_IMAGE -f Dockerfile.$DOCKER_IMAGE .
```
followed by
```sh
DOCKER_IMAGE=alpine-jdk11-chromium
docker build -t $DOCKER_IMAGE -f Dockerfile.$DOCKER_IMAGE .
docker run -it $DOCKER_IMAGE
```
which logs
```sh
WARNING: Unable to find an exact match for CDP version 86, so returning the closest version found: 84
Nov 20, 2020 2:57:34 AM org.openqa.selenium.devtools.CdpVersionFinder findNearestMatch
INFO: Found CDP implementation for version 86 of 84
PortProber findFreePort -> 9973
Hi, Julio
```
### See Also

  * https://www.cyberciti.biz/faq/10-alpine-linux-apk-command-examples/

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
