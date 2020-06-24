### Info


This directory contains subset of classes extracted from IBM
[ibm-ucdeploy-publisher.hpi](https://www.ibm.com/support/pages/how-integrate-urbancode-deploy-jenkins-continuous-integration)
[source code repo](https://github.com/UrbanCode/jenkins-publisher-ucd-plugin) , which has a number of type helpers, packaged as dependency jars

```sh
  uc-codestation-aggregate-1.0-SNAPSHOT.jar
  uc-commons-fileutils-1.0-SNAPSHOT.jar
  uc-commons-util-1.0-SNAPSHOT.jar
  uc-httpcomponents-util-1.0-SNAPSHOT.jar
  uc-jersey-client-1.0-SNAPSHOT.jar
  uc-jersey-core-1.0-SNAPSHOT.jar
  uc-jettison-1.0-SNAPSHOT.jar
  uc-jsr311-api-1.0-SNAPSHOT.jar
  uc-replicated-codestation-1.0-SNAPSHOT.jar
  uc-uDeployRestClient-1.0-SNAPSHOT.jar
```

and may be useful to follow best practices instead of re-inventing a wrapper around __UrbanCode Deploy Rest client__.
Note: UrbanCode Deploy Rest client is not published to Maven repository - 
`https://mvnrepository.com/search?q=uDeployRestClient` will return nothing, so one has to build it oneself.


The dependencies have never been published into Maven central, so to have them locally
Download and extract  the hpi from `https://www.urbancode.com/uc-downloads/plugins/ibmucd/jenkins-ud-plugin/`:

```sh
wget https://www.urbancode.com/uc-downloads/plugins/ibmucd/jenkins-ud-plugin/ibm-ucdeploy-publisher-1.14.1055775.hpi
unzip ibm-ucdeploy-publisher-1.14.1055775.hpi
find WEB-INF -iname 'uc*jar'
```


Then modify the original pom.xml by switching to system so:
```xml
<dependency>
  <groupId>uc-jersey-client</groupId>
  <artifactId>uc-jersey-client</artifactId>
  <version>1.0-SNAPSHOT</version>
</dependency>
```
becomes
```xml
<dependency>
  <groupId>uc-jersey-client</groupId>
  <artifactId>uc-jersey-client</artifactId>
  <version>1.0-SNAPSHOT</version>
  <scope>system</scope>
  <systemPath>${project.basedir}/WEB-INF/lib/uc-jersey-client-1.0-SNAPSHOT.jar</systemPath>
</dependency>
```
for every non-maven-published vendor dependency.

Then remove everything but a few files from the original project:
`RestClientHelper.java` , `UrbanDeploySite.java` remain, everything else temporarily removed, and prune them from hudson type dependencies.


NOTE: IBM also developed and published [source code](https://github.com/UrbanCode/jenkins-pipeline-ucd-plugin)  of Jenkins pipeline
version which has more functionality in particular publishes versions. It also have good version of dependency jars.

### Accessing UCD Server

We intend to  be interacting with a vanilla trial version of the [UrbanCode Deploy Server](https://hub.docker.com/r/ibmcom/ucds/) Docker container
To create one
### Usage
```sh
docker pull ibmcom/ucds
```
NOTE: Docker image is heavy (1.03GB per docker image after pull, 1.7 Gb consumed after the install finished)
and initial run is *very* time-consuming

```sh
NAME='ucd-server'
IMAGE='ibmcom/ucds'
IMAGE_ID=$(docker image ls | grep $IMAGE |awk '{print $3}')
```
NOTE: IBM documentation is slightly incorrect by advising run container daemonized.

```sh
docker run --name $NAME -d -p 8443:8443 -p 7918:7918 -p 8080:8080 -t $IMAGE_ID
```
when done so, subsequent call
```sh
docker logs $NAME
```
reveals
```sh
Installing IBM UrbanCode Deploy...
Started building ucds container image at Sat Jun 20 18:36:38 UTC 2020.
unzip -q /tmp/ibm-ucd-6.2.7.1.960481.zip -d /tmp
[Database] Configuring Derby
Completed preparing ucds install files at Sat Jun 20 18:40:25 UTC 2020.
Enter the directory of the server to upgrade(leave blank for installing to a clean directory).
```
and subsquent check
```sh
while true ; do docker logs $NAME ; sleep 60 ; done
```
reveals no progress beyond that point. It needs an interactive run!

```sh
docker container stop $NAME
docker container rm $NAME
docker container prune -f
```
- may need an extra retry
```sh
docker run --name $NAME -it -p 8443:8443 -p 7918:7918 -p 8080:8080 -t $IMAGE_ID
```
May need to enter if there is no progess visible in the console
first an iteractive login and password change is required
```sh
https://localhost:8443
```
(use the target machine ip if it is not accessed locally)
as __admin__/__admin__

```sh
docker start $(docker container ls -a | grep 'ucd-server' | awk '{print $1}')
```
this will restore port mapping


#### Test Rest Client

*  copy dependency jars into current  directory
```sh
JARS='uc-uDeployRestClient-1.0-SNAPSHOT.jar commons-codec-1.5.jar uc-jettison-1.0-SNAPSHOT.jar'
for JAR in $JARS; do cp WEB-INF/lib/$JAR . ; done
```
*  compile
```sh
javac -cp uc-uDeployRestClient-1.0-SNAPSHOT.jar:commons-codec-1.5.jar:uc-jettison-1.0-SNAPSHOT.jar BasicAgentClientTest.java
```
*  run
```sh
java -cp uc-uDeployRestClient-1.0-SNAPSHOT.jar:commons-codec-1.5.jar:uc-jettison-1.0-SNAPSHOT.jar:. BasicAgentClientTes
```
when  invalid credentials provided one will get an exception (converted to Runtme one):

```
```
when credentials are correct the excetion will become
```sh
Exception in thread "main" java.io.IOException: 404
No agent with id/name dummy
```
### Building as and Using Package
* adding one exta class solely to parse commandline arguments, makes it sometimes easier to
package into `jar` with manifest 
```sh
mvn clean package
```
followed by
```sh
java -cp target/example.ucdclient.jar:target/lib/*  example.BasicAgentClientTest  -debug -agent dummy
```
### See Also

  * https://github.com/UrbanCode/UCD-Docker-Images
  * REST client [ucd](https://github.com/UrbanCode/uDeployRestClient)
  * Jenkins pipeline [ucd plugin](https://github.com/UrbanCode/jenkins-pipeline-ucd-plugin) source code
  * Usage of __component\_deployment__ and __version\_import__  into [ucd](https://www.urbancode.com/plugindoc/jenkins-pipeline#tab-usage) from Jenkins, Pipeline syntax 
  * https://github.com/UrbanCode/Jenkins-Job-Manager-UCD
  
