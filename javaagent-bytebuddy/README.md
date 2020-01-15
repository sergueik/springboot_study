### Info

This directory contains a fragment replica of [javaagent-bytebuddy](https://github.com/ShehanPerera/javaagent-bytebuddy) project by ShehanPerera: bare bones Java Agent agent using [byte-buddy](https://bytebuddy.net/#/) code generation and manipulation library for attaching an agent to the "business application" jar and Maven [shade plugin](https://maven.apache.org/plugins/maven-shade-plugin/).

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

java -javaagent:agent/target/get-methods-1.0-SNAPSHOT.jar -jar application/target/app-0.1-SNAPSHOT.jar
```
This will print a lot of detailed instrumentation logging to the console. Connecting to the linked Docker image to publish on vanilla WEB server is work in progress


### See Also

  * https://docs.oracle.com/javase/7/docs/api/java/lang/instrument/package-summary.html
  * https://docs.appdynamics.com/display/PRO45/Install+the+Java+Agent
  * https://www.oracle.com/technetwork/articles/java/javamanagement-140525.html


### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
