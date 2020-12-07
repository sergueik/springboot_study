### Info

This directory ocontains subset of classes extracted from IBM
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

for building a simple inventory client not all of the above is needed, just the
```sh
uc-uDeployRestClient-1.0-SNAPSHOT.jar
commons-codec-1.5.jar
uc-jettison-1.0-SNAPSHOT.jar
```
to enable intellisense in Eclipse add the following to the `.classpath`:
```xml
<classpathentry kind="lib" path="WEB-INF/lib/uc-uDeployRestClient-1.0-SNAPSHOT.jar"/>
<classpathentry kind="lib" path="WEB-INF/lib/uc-jettison-1.0-SNAPSHOT.jar"/>
```
and `commons-codec` to `pom.xml`

For UCD web interface see [Simple UrbanCode Deploy tutorial](https://www.ibm.com/support/knowledgecenter/SS4GSP_7.0.5/com.ibm.udeploy.tutorial.doc/topics/quickstart_abstract.html)


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

* pull and launch server image if not already

```sh
SERVER_IMAGE='ibmcom/ucds'
docker pull $SERVER_IMAGE
```
NOTE: The Docker image is heavy (1.03GB per docker image after pull, 1.7 Gb consumed after the install finished)
and initial auto-bootstrap run is *very* time-consuming

 * check if the container was already creatd and stopped and start in case (this will save quite a lot of time of initial setup):

```sh
SERVER_NAME='ucd-server'
docker container ls -a | grep $SERVER_NAME
IMAGE=$(docker container ls -a | grep $SERVER_NAME | cut -f 1 -d ' ' | )
echo $IMAGE|xargs -IX docker container start X
docker logs $IMAGE
```
when seen
```sh
UTC INFO  main com.urbancode.ds.UDeployServer - IBM UrbanCode Deploy server version 6.2.7.1.960481 started.
```
it can be used right away. Otherwise, build it
```sh
SERVER_NAME='ucd-server'
SERVER_IMAGE='ibmcom/ucds'
SERVER_ID=$(docker image ls -a | grep $SERVER_IMAGE |awk '{print $3}')
```
NOTE: IBM documentation is slightly incorrect by advising run container daemonized.

```sh
docker run --name $SERVER_NAME -d -p 8443:8443 -p 7918:7918 -p 8080:8080 -t $SERVER_ID
```
when done so, subsequent call
```sh
docker logs $SERVER_NAME
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
while true ; do docker logs $SERVER_NAME ; sleep 60 ; done
```
reveals no progress beyond that point. It needs an interactive run!
There appears to be no way to put a carriage return into the process stdin (is there?).
```sh
docker container stop $SERVER_NAME
docker container rm $SERVER_NAME
docker container prune -f
```
- may need an extra retry
```sh
docker run --name $SERVER_NAME -it -p 8443:8443 -p 7918:7918 -p 8080:8080 -t $SERVER_ID
```
it will show at least two prompts:
```sh
Enter the directory of the server to upgrade(leave blank for installing to a clean directory).

Enter the home directory for the JRE/JDK that the new server or already installed server uses. Default [/opt/ibm/java/jre]:

[echo] Enter the database type to use. [Default: derby]
[echo] Enter the database username. [Default: ibm_ucd]
[echo] Enter the database password. [Default: password]
```
After a long runnning install process eventually culminated with
```sh
INFO  main com.urbancode.ds.UDeployServer - IBM UrbanCode Deploy server version 6.2.7.1.960481 started.
```
May need to enter if there is no progess visible in the console
first an iteractive login and password change is required
```sh
https://localhost:8443
```
(use the host machine ip address if it is not accessed locally)
as __admin__/__admin__

```sh
SERVER_NAME='ucd-server'
docker start $(docker container ls -a | grep $SERVER_NAME | awk '{print $1}')
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

```sh
TBD
```
when credentials are correct the exception will become
```sh
Exception in thread "main" java.io.IOException: 404
No agent with id/name dummy
```

* pull and launch ucd agent
```sh
export CLIENT_IMAGE='ibmcom/ucda'
docker pull $CLIENT_IMAGE
export SERVER_NAME='ucd-server'
```
then bind it to the Server

```sh
SERVER_ID=$( docker container ls -a| grep $SERVER_NAME | awk '{print $1}')
docker ps | grep $SERVER_ID
```
if not shown as running, start it
```sh
docker start $SERVER_ID
docker inspect --format='{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' $SERVER_ID
```
alternaively (`/sbin/ip` may actually be unavailable on ucd-server),
```sh
docker exec -t $SERVER_ID /bin/sh -c '/sbin/ip address show eth0 |grep inet'
```
or
```sh
docker exec -t $SERVER_ID /bin/sh -c '/sbin/ifconfig eth0|grep inet'
```
or
```sh
docker inspect $SERVER_ID | jq -r '.[]|.NetworkSettings.Networks.bridge.IPAddress'
```

* export the value manually (temporarily)

```sh
export UCD_SERVER_IP=172.17.0.2
```
to examine the earlier launched agent, e.g. to inspect installed jars, start it and launch shell there
```sh
docker run -d $CLIENT_IMAGE
C=$( docker container ls -a| grep $CLIENT_IMAGE | awk '{print $1}')
```
if it is the first time it is run,
```sh
docker logs $CLIENT_ID
docker exec -it $CLIENT_ID sh
```
there
```sh
hostname -i
```
otherwise
```sh
docker start $CLIENT_ID
```
* in the local shell there
```sh
ps ax| grep agen[t].sh
```
this shows the entrypoint shell script to be `/entrypoint-ibm-ucd-agent.sh`
and
```sh
ps ax| grep com.urbancode.ai[r] | sed 's|  *|\n|g'
```

will see that the main class name is `com.urbancode.air.agent.AgentWorker` and the classpath is
in defined in `/opt/ibm-ucd/agent/bin/classpath.conf` as
```sh
dir /opt/ibm-ucd/agent/conf/agent
dir /opt/ibm-ucd/agent/conf
set /opt/ibm-ucd/agent/lib
nativedir /opt/ibm-ucd/agent/native/x64
```

the `/opt/ibm-ucd/agent/lib` dir has the following dependency jars:
```sh
ActiveMQTransport.jar
Commons-Validation.jar
CommonsFileUtils.jar
CommonsUtil.jar
CommonsXml.jar
HttpComponents-Util.jar
NativeProcess.jar
SSHClient.jar
SandboxedJSRuntime.jar
WinAPI.jar
activemq-core-5.6.0.jar
air-agent.jar
air-agentmanager.jar
air-devilfish.jar
air-diagnostics.jar
air-keytool.jar
air-net.jar
air-plugin-command.jar
air-propertyresolver.jar
air-service-event.jar
anthill3-logic.jar
bcpkix-jdk15on-147.jar
bcprov-jdk15on-147.jar
bridge-method-annotation.jar
bridge-method-injector.jar
bsf.jar
commons-codec.jar
commons-collections.jar
commons-detection.jar
commons-io.jar
commons-lang.jar
commons-lang3.jar
commons-logfile.jar
commons-logfiletool.jar
commons-logging.jar
fluent-hc.jar
geronimo-j2ee-management_1.1_spec-1.0.1.jar
groovy-all-1.8.8.jar
httpclient-cache.jar
httpclient.jar
httpcore.jar
httpmime.jar
jcl-over-slf4j.jar
jettison-1.1.jar
jms.jar
js.jar
jsch.jar
log4j.jar
netty-all.jar
securedata.jar
shell.jar
slf4j-api.jar
slf4j-log4j12.jar
spring-beans.jar
spring-context.jar
spring-core.jar
spring-jdbc.jar
spring-tx.jar
xbean-spring.jar
```

these jars can be used in custom scripting (there isn't much)


* otherwise just restart it
```sh
export UCD_SERVER_IP=172.17.0.2
docker stop $CLIENT_ID
# docker prune -f
docker run -d  --name 'ucd_agent' --add-host="ucd-server:$UCD_SERVER_IP" -t $CLIENT_IMAGE
```
* open server `https://172.17.0.2:8443` in browser
and observe agent (`agent-c0336485ef9d`) discovered
```sh
curl -k -u admin:admin https://172.17.0.2:8443/rest/resource/resource | \
jq -cr '.[]|select(.description == "Agent")|.name'
```
this will give:
```text
agent-09fcc2c48575
```

or (assuming some properties and components were added to that agent):
```sh
RESOURCE_PATH=/TEST/agent-09fcc2c48575
curl -X GET -k -u admin:admin https://172.17.0.2:8443/cli/resource/getProperties?resource=$RESOURCE_PATH| jq '.'
```
```json
[
  {
    "id": "1763af4e-e018-d41c-0aa4-5220b78d0710",
    "name": "property_name_1",
    "value": "value_1",
    "description": "",
    "secure": false
  }
]
```
and
```sh
 curl -X GET -k -u admin:admin "https://172.17.0.2:8443/cli/resource/getProperty?resource=$RESOURCE_PATH&name=property_name_1"
```
```text
value_1
```
```sh
curl -k -u admin:admin "https://172.17.0.2:8443/cli/resource?parent=$RESOURCE_PATH" | jq -r '.[]|"name= " +.name + "\n" + "path= " + .path'
```
this will give:

```text
name= component_1
path= /TEST/agent-09fcc2c48575/component_1
name= component_2
path= /TEST/agent-09fcc2c48575/component_2
name= component_3
path= /TEST/agent-09fcc2c48575/component_3
```
```
curl -k -u admin:admin "https://172.17.0.2:8443/cli/resource?parent=$RESOURCE_PATH" | jq '.
```

one can also remove those resources in a REST fashion:
```sh
curl -k -u admin:admin -X DELETE "https://172.17.0.2:8443/cli/resource/deleteResource?resource=/TEST/agent-09fcc2c48575/component_1"
```
or (may not work)

```sh
curl -H 'Authorization: Basic YWRtaW46YWRtaW4K' -k https://localhost:8443/rest/resource/resource
```

### Elementary Tests
These can run from either Linux or Windows node
```sh
mvn package
```
```sh
java -cp target/example.ucdclient.jar:target/lib/* example.GetResourceByPath -path "/TEST"
java -cp target/example.ucdclient.jar:target/lib/* example.GetResourceByPath -id 173792cf-acfd-bfc9-7df1-f3d8a190d6b7
java -cp target/example.ucdclient.jar:target/lib/* example.GetResourceByPath -path 173792cf-acfd-bfc9-7df1-f3d8a190d6b7
```
will all reply
```sh
id: "173792cf-acfd-bfc9-7df1-f3d8a190d6b7"
name: "TEST"
path: "/TEST"
description: "Test group"
```
on Windows client modify the command line and provide the `server` parameter:
```cmd
java -cp target\example.ucdclient.jar;target\lib\* example.GetResourceByPath -path 173792cf-acfd-bfc9-7df1-f3d8a190d6b7 -server https://192.168.0.64:8443/
```
this will reply
```sh
id: "173792cf-acfd-bfc9-7df1-f3d8a190d6b7"
name: "TEST"
path: "/TEST"
description: "Test group"
```

### Resource Tests
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
    $ial run of Udeply server is remarkably time consuming and the docker image isabout to exprie Feb 11 2021.
There is no out of the box say to preserve

* Create a [backup](https://docs.docker.com/engine/reference/commandline/save/)
```sh
docker start $CLIENT_ID $SERVER_ID
CLIENT_IMAGE=$(docker container inspect $CLIENT_ID | jq -r '.[0]|.Name')
SERVER_IMAGE=$(docker container inspect $SERVER_ID | jq -r '.[0]|.Name')
```
```sh
docker export --output ucd_agent.tar "$CLIENT_IMAGE"
docker export -o ucd_server.tar $SERVER_ID
gzip -9 ucd_agent.tar ucd_server.tar
```
### See Also

  * https://github.com/UrbanCode/UCD-Docker-Images
  * REST client [ucd](https://github.com/UrbanCode/uDeployRestClient)
  * Jenkins pipeline [ucd plugin](https://github.com/UrbanCode/jenkins-pipeline-ucd-plugin) source code
  * Usage of __component\_deployment__ and __version\_import__ into [ucd](https://www.urbancode.com/plugindoc/jenkins-pipeline#tab-usage) from Jenkins, Pipeline syntax
  * https://github.com/UrbanCode/Jenkins-Job-Manager-UCD
  * https://freddysf.wordpress.com/2013/12/05/urbancode-deploy-agent-based-source-config-types/
  * https://db.apache.org/derby/
  * https://stedolan.github.io/jq/manual/#ConditionalsandComparisons

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

