### Info

This directory contains a fragment replica of [javaagent-bytebuddy](https://github.com/ShehanPerera/javaagent-bytebuddy) project by ShehanPerera: JavaAgent basics and experiment with Docker.


### Testing

NOTE: the instrumented application is not assumed to share the code base with agent, so we split them apart.
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
 * Deploy to Docker image to gether (work in progress)
 * run instrumented application:
```sh

java -javaagent:agent/target/get-methods-1.0-SNAPSHOT.jar -jar application/target/example-run-1.0-SNAPSHOT.jar
```
This will print a lot of detailed instrumentation logging to the console. Connecting to the linked docker image to publish on vanilla WEB server is work in progress


### See Also

  * https://docs.oracle.com/javase/7/docs/api/java/lang/instrument/package-summary.html
  * https://docs.appdynamics.com/display/PRO45/Install+the+Java+Agent

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
