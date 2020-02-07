### Info

This directory contains a fragment replica of [javaagent-bytebuddy](https://github.com/ShehanPerera/javaagent-bytebuddy) project by ShehanPerera: bare bones Java Agent agent
that uses [byte-buddy](https://bytebuddy.net/#/) code generation and manipulation library for attaching an agent to the "business application" jar and
Maven [shade plugin](https://maven.apache.org/plugins/maven-shade-plugin/usage.html) `org.apache.maven.plugins.shade.resource.ManifestResourceTransformer` 
to write the main manifest `MANIFEST.MF` entries (to prevent from "no main manifest attribute, in target application jar" error).

This is a work in progress experiment the same with Docker linked to dedicated Docker insance serving for agent messages sink and to replace a vanilla app jar with an equally vanilla Springboot REST server.


### Testing

NOTE: the instrumented application is not assumed to share the code base with agent, so we take them apart.
 * Build agent
```sh
pushd  agent
mvn clean package
popd
```
 * Build application
```sh
pushd appplication
mvn clean package
popd
```
 * Deploy both jars into Docker container together (work in progress)
 * run instrumented application bundle:

```sh
java -javaagent:agent/target/agent.jar -jar application/target/application.jar
```


This will print a lot of detailed instrumentation logging to the console.
Connecting to the linked Docker image to publish on vanilla WEB server is work in progress

### Run on Docker

* repackage  or use already packaged earlier
```sh
pushd  agent
mvn package
cd  ../appplication
mvn package
popd
```

* containerize
```sh
docker build -t 'application-agent' .
```
* launch and remove container
```sh
docker run --rm 'application-agent'
docker image rm 'application-agent'
```

### See Also

  * https://docs.oracle.com/javase/7/docs/api/java/lang/instrument/package-summary.html
  * https://docs.appdynamics.com/display/PRO45/Install+the+Java+Agent
  * https://www.oracle.com/technetwork/articles/java/javamanagement-140525.html
  * [multiple services in a Docker container](https://docs.docker.com/config/containers/multi-service_container/)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
