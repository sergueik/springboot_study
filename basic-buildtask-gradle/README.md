### Info

trimmed replica of
[https://www.baeldung.com/gradle-custom-task](https://www.baeldung.com/gradle-custom-task) example

### Docker Alpine Test

* build, specifying the base image via `build-arg`, note the syntax)
```sh
IMAGE=basictask-gradle
BASE=gradle:5.4.1-jdk8-alpine
docker pull $BASE
docker build --build-arg "BASE=$BASE" -t $IMAGE -f Dockerfile .
```
* run (omit the `--rm` flag and optionally add provide the `--name` flag with argument if intended to keep the container)
```sh
docker container run -u root --rm -it $IMAGE
```
repeat with seting `BASE` to `gradle:8.1-jdk11-alpine`,`gradle:7.3.1-jdk17-alpine`

You will get similar output (sans the banner) on Gradle 5.x, Gradle 7.x and Gradle 8.x:
```text
Starting a Gradle Daemon (subsequent builds will be faster)
> Task :buildSrc:compileJava
> Task :buildSrc:compileGroovy
> Task :buildSrc:processResources
> Task :buildSrc:classes
> Task :buildSrc:jar
> Task :buildSrc:assemble
> Task :buildSrc:compileTestJava NO-SOURCE
> Task :buildSrc:compileTestGroovy NO-SOURCE
> Task :buildSrc:processTestResources NO-SOURCE
> Task :buildSrc:testClasses UP-TO-DATE
> Task :buildSrc:test NO-SOURCE
> Task :buildSrc:check UP-TO-DATE
> Task :buildSrc:build
> Task :clean UP-TO-DATE
> Task :compileJava NO-SOURCE
> Task :compileGroovy NO-SOURCE
> Task :processResources NO-SOURCE
> Task :classes UP-TO-DATE
> Task :jar
> Task :assemble
> Task :compileTestJava NO-SOURCE
> Task :compileTestGroovy NO-SOURCE
> Task :processTestResources NO-SOURCE
> Task :testClasses UP-TO-DATE
> Task :test NO-SOURCE
> Task :check UP-TO-DATE
> Task :build

> Task :updateUdeployProperties
reading template configuration from file: /work/buildSrc/src/main/resources/application.yaml
template configuration: ---
property1: {{* name1||default1 *}}
property2: {{* name2||default2 *}}
property3: {{* name3||default3 *}}
property4: {{* name4||default4 *}}
# comment
property5: value5

new configuration: ---
property1: https://www.yahoo.com
property2: user
property3: 17
property4: default4
# comment
property5: value5
new configuration: ---
property1: https://www.yahoo.com
property2: user
property3: 17
property4: default4
# comment
property5: value5
Written content to /work/buildSrc/src/main/resources/application.yaml succesfully!
Done.

> Task :updateApplicationProperties
filePath: buildSrc/src/main/resources
reading template configuration from file: /work/buildSrc/src/main/resources/application.properties
template configuration: commandline: name1=https://www.google.com name2=admin name3=42 name4=""
example: "${name1} ${name2} ${name3} ${name4}"
new configuration: commandline: name1=https://www.google.com name2=admin name3=42 name4=""
example: "https://www.google.com admin 42 """
Written content to /work/buildSrc/src/main/resources/application.properties succesfully!
Done.

BUILD SUCCESSFUL in 6m 29s
4 actionable tasks: 3 executed, 1 up-to-date
```

To review the updated files, keep the container and print the file after. To troubleshoot, run shell in the container

```sh
NAME=basictask-gradle
docker container run --name $NAME -u root -it $IMAGE sh
```
```sh
gradle clean build updateUdeployProperties updateApplicationProperties
```
```sh
cat buildSrc/src/main/resources/application.yaml
```
```text
---
property1: https://www.yahoo.com
property2: user
property3: 17
property4: default4
# comment
property5: value5
```

```sh
exit
```
```sh
docker stop $NAME 
docker container rm $NAME
```

* NOTE: one may find it useful to increase the Docker machine system to two CPU when testing the gradle tasks

![Docker Machine](https://github.com/sergueik/springboot_study/blob/master/basic-buildtask-gradle/screenshots/capture-docker-machine.png)

### Troubleshooting
* NOTE: if troublehooting, run shell
```sh
docker container run -u root -it $IMAGE sh
```
in the container

```sh
gradle clean build updateApplicationProperties
```
or 
```sh
gradle clean build updateUdeployProperties
```
### Cleanup
```sh
docker image prune -f 
docker volume prune -f
```
### NOTE:
cannot add
```sh
RUN  gradle build
```
instruction to the `Dockerfile`: getting the fake build error and image is marked as not created:
```text
FAILURE: Build failed with an exception.

* What went wrong:
Could not create service of type ScriptPluginFactory using BuildScopeServices.cr
eateScriptPluginFactory().
> Could not create service of type CrossBuildFileHashCache using BuildSessionSco
peServices.createCrossBuildFileHashCache().

```
### Test Standalone

```sh
gradle clean build updateApplicationProperties
```
it will update configuration and log the old and new contents to console

```text
> Task :updateApplicationProperties
> Task :updateApplicationProperties
Processing tool: java
new configuration: ---
property1: {{* name1||default1 *}}
property2: {{* name2||default2 *}}
property3: {{* name3||default3 *}}
property4: {{* name4||default4 *}}
# comment
property5: value5
new configuration: ---
property1: https://www.google.com
property2: admin
property3: 42
property4: default4
# comment
property5: value5
new configuration: ---
property1: https://www.google.com
property2: admin
property3: 42
property4: default4
# comment
property5: value5
```
then print an environment variable:
```text
1.8.0_161
```
* repeat building two tasks with different parameters
```sh
gradle clean build updateApplicationProperties updateUdeployProperties
```
```
> Task :updateApplicationProperties
Processing tool: groovy
new configuration: ---
property1: {{* name1||default1 *}}
property2: {{* name2||default2 *}}
property3: {{* name3||default3 *}}
property4: {{* name4||default4 *}}
# comment
property5: value5
new configuration: ---
property1: https://www.google.com
property2: admin
property3: 42
property4: default4
# comment
property5: value5
new configuration: ---
property1: https://www.google.com
property2: admin
property3: 42
property4: default4
# comment
property5: value5
TODO: GroovySystem.getVersion()
Done.
```
```
> Task :updateUdeployProperties
Processing tool: groovy
new configuration: ---
property1: {{* name1||default1 *}}
property2: {{* name2||default2 *}}
property3: {{* name3||default3 *}}
property4: {{* name4||default4 *}}
# comment
property5: value5
new configuration: ---
property1: https://www.yahoo.com
property2: user
property3: 17
property4: default4
# comment
property5: value5
new configuration: ---
property1: https://www.yahoo.com
property2: user
property3: 17
property4: default4
# comment
property5: value5
TODO: GroovySystem.getVersion()
Done.
```
ignore the warnings:
``` 
Deprecated Gradle features were used in this build, making it incompatible with Gradle 7.0.
Use '--warning-mode all' to show the individual deprecation warnings.
See https://docs.gradle.org/6.6.1/userguide/command_line_interface.html#sec:command_line_warnings
```
```
BUILD SUCCESSFUL in 1s
4 actionable tasks: 4 executed
```
### See Also
  * https://www.linkedin.com/pulse/writing-custom-gradle-plugin-using-java-mahendra-tonape/
  * [gretl plugin](https://github.com/sogis/gretl)
  * https://github.com/gpetuhov/SampleGradle
DefaultTask  * example [Multi Module Gradle project with Custom Gradle Plugin and Custom task](https://github.com/TechPrimers/gradle-custom-plugin-example-2)
  * https://docs.gradle.org/current/userguide/more_about_tasks.html
  * https://docs.gradle.org/current/userguide/custom_tasks.html#sec:writing_a_simple_task_class
  * https://docs.gradle.org/current/javadoc/org/gradle/api/tasks/Optional.html
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
