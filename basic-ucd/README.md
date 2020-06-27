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

![IBM Urbancode Udeploy Server Example](https://github.com/sergueik/springboot_study/blob/master/basic-ucd/screenshots/agent.png)

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

We intend to  be interacting with a vanilla trial version of the [IBM UrbanCode Deploy Server](https://hub.docker.com/r/ibmcom/ucds/)
and [IBM UrbanCode Deploy Agent](https://hub.docker.com/r/ibmcom/ucda/)
Docker containers
To create one

### Usage
* pull and launch server
```sh
docker pull ibmcom/ucds
```
NOTE: The Docker image is heavy (1.03GB per docker image after pull, 1.7 Gb consumed after the install finished)
and initial auto-bootstrap run is *very* time-consuming

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
NAME='ucd-server'
docker start $(docker container ls -a | grep $NAME | awk '{print $1}')
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
java -cp uc-uDeployRestClient-1.0-SNAPSHOT.jar:commons-codec-1.5.jar:uc-jettison-1.0-SNAPSHOT.jar:. BasicAgentClientTest  --agent dummy
```
when  invalid credentials provided one will get an exception (converted to Runtme one):

```
```
when credentials are correct the exception will become
``sh
Exception in thread "main" java.io.IOException: 404
No agent with id/name dummy
```

* pull and launch ucd agent
```sh
NAME='ucd-server'
docker pull $IMAGE
```
then bind it to the Server

```sh
docker inspect --format='{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' $(  docker container ls | grep $NAME | awk '{print $1}' )
```
* collect the value manually (temporarily)

```
IMAGE='ibmcom/ucda'
export UCD_SERVER_IP=172.17.0.3
docker run -d --add-host="ucd-server:$UCD_SERVER_IP" -t $IMAGE
```
* open server `https://172.17.0.2:8443` in browser
and observe agent (`agent-c0336485ef9d`) discovered

* run the agent inventory
from linux machine
```sh
# use environment to pass password
export my_password=admin
java -cp target/example.ucdclient.jar:target/lib/* example.BasicAgentClientTest -env '172ecdb3-50a2-e489-6b50-1399b396b6fb' -password env:secret_password
```
from Windows machine
```cmd
set SECRET_PASSWORD=admin
java -cp target\example.ucdclient.jar;target\lib\* example.BasicAgentClientTest -env "172ecdb3-50a2-e489-6b50-1399b396b6fb" -server https://192.168.0.64:8443 -password env:SECRET_PASSWORD
```
can use any name you like instead of `SECRET_PASSWORD`, as long as it is consistent between the command setting it and the argument name retriveing it


this will produce

```sh
 - agent-ad84625cbc4b
   - Component  2
   - Dummy Component
```      
and when `debug` flag is set, more raw json like:      
```json
{
  "id": "172ebdc2-8423-fc4e-a0f8-20a2be97f2b9",
  "securityResourceId": "172ebdc2-822c-aab3-4762-3a688629aece",
  "name": "agent-c0336485ef9d",
  "active": true,
  "licensed": false,
  "licenseType": "NONE",
  "status": "ONLINE",
  "version": "6.2.7.1.960390",
  "lastContact": 1593095454348,
  "dateCreated": 1593094973506,
  "workingDirectory": "/opt/ibm-ucd/agent/var/work/",
  "impersonationPassword": "****",
  "impersonationUseSudo": false,
  "impersonationForce": false,
  "tags": [],
  "security": {
    "read": true,
    "execute": true,
    "Add to Agent Pool": true,
    "Create Resources": true,
    "Delete": true,
    "Edit Basic Settings": true,
    "Execute on Agents": true,
    "Install Remote Agents": true,
    "Manage Impersonation": true,
    "Manage Properties": true,
    "Manage Teams": true,
    "Manage Version Imports": true,
    "Upgrade Agents": true,
    "View Agents": true
  },
  "propSheet": {
    "id": "172ebdc2-8433-3be7-1e08-78efe59afc49",
    "path": "agents/172ebdc2-8423-fc4e-a0f8-20a2be97f2b9/propSheet",
    "version": 2,
    "versionCount": 2,
    "commit": 4,
    "versioned": true
  },
  "extendedSecurity": {
    "read": true,
    "execute": true,
    "Add to Agent Pool": true,
    "Create Resources": true,
    "Delete": true,
    "Edit Basic Settings": true,
    "Execute on Agents": true,
    "Install Remote Agents": true,
    "Manage Impersonation": true,
    "Manage Properties": true,
    "Manage Teams": true,
    "Manage Version Imports": true,
    "Upgrade Agents": true,
    "View Agents": true,
    "teams": []
  }
}
```
### Building Agent Client as and running package
* adding one extra class solely to parse commandline arguments, makes it sometimes easier to
package into `jar` with manifest 
```sh
mvn clean package
```
followed by
```sh
java -cp target/example.ucdclient.jar:target/lib/*  example.BasicAgentClientTest  -debug -agent dummy
```
After associating Component with Agent in UCD, the agent inventory JSON does not show any change.
However the call to
```java
// explore resource hierarchy
 ResourceClient r = new ResourceClient(new URI("https://localhost:8443"), user, password);
 String id ="172ecdb3-50a2-e489-6b50-1399b396b6fb";
 JSONArray children = r.getResourceChildren(id);
 // System.out.println("{\"" + id + "\": "+ children + " }");
 id = "172ecdb9-54f7-c269-9cca-fd8bd9ee6341";
 children= r.getResourceChildren(id);
 System.out.println("{\"" + id1 + "\": "+ children + " }");
```
reveal the hoerarchy of resources:
```js
{
  "172f2041-bfa1-c87d-e2cd-f0f9bb270040": [
    {
      "id": "172f2046-3c18-c275-a13d-8e0f77475303",
      "securityResourceId": "172f2046-3be0-2e16-1428-af4094c36d48",
      "name": "Component  2",
      "path": "/TEST/agent-14401f33ae44/Component  2",
      "active": true,
      "description": "",
      "inheritTeam": true,
      "discoveryFailed": false,
      "prototype": false,
      "impersonationPassword": "****",
      "impersonationUseSudo": false,
      "impersonationForce": false,
      "type": "subresource",
      "status": "ONLINE",
      "hasAgent": true,
      "tags": []
    }
  ],
  "172f1c3c-8bcd-ca06-0e12-1331c3c8a0c1": [
    {
      "id": "172f1d46-ba77-1d34-73d1-149e8b2c6a80",
      "securityResourceId": "172f1d46-b8c6-c626-715f-834d913ee701",
      "name": "Component  2",
      "path": "/TEST/agent-2181a1920431/Component  2",
      "active": true,
      "description": "",
      "inheritTeam": true,
      "discoveryFailed": false,
      "prototype": false,
      "impersonationPassword": "****",
      "impersonationUseSudo": false,
      "impersonationForce": false,
      "type": "subresource",
      "status": "ONLINE",
      "hasAgent": true,
      "tags": []
    },
    {
      "id": "172f1c98-4dda-04c7-e61c-92b6d9e2a9d6",
      "securityResourceId": "172f1c98-4bb6-5693-ef34-7d91c929466d",
      "name": "Dummy Component",
      "path": "/TEST/agent-2181a1920431/Dummy Component",
      "active": true,
      "description": "",
      "inheritTeam": true,
      "discoveryFailed": false,
      "prototype": false,
      "impersonationPassword": "****",
      "impersonationUseSudo": false,
      "impersonationForce": false,
      "type": "subresource",
      "status": "ONLINE",
      "hasAgent": true,
      "tags": []
    }
  ]
}
```

![IBM Urbancode Udeploy Server Example](https://github.com/sergueik/springboot_study/blob/master/basic-ucd/screenshots/configured_agent_capture.png)


### See Also

  * https://github.com/UrbanCode/UCD-Docker-Images
  * REST client [ucd](https://github.com/UrbanCode/uDeployRestClient)
  * Jenkins pipeline [ucd plugin](https://github.com/UrbanCode/jenkins-pipeline-ucd-plugin) source code
  * Usage of __component\_deployment__ and __version\_import__  into [ucd](https://www.urbancode.com/plugindoc/jenkins-pipeline#tab-usage) from Jenkins, Pipeline syntax 
  * https://github.com/UrbanCode/Jenkins-Job-Manager-UCD
