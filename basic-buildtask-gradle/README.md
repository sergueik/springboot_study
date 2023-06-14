### Info

trimmed replica of
[https://www.baeldung.com/gradle-custom-task](https://www.baeldung.com/gradle-custom-task) example

### Usage

```sh
gradle clean build printJavaVersionBuildSrc
```
it will update configuration and log the old and new contents to console

```text
> Task :printJavaVersionBuildSrc
> Task :printJavaVersionBuildSrc
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
gradle clean build printJavaVersionBuildSrc printGroovyVersionBuildSrc
```
```
> Task :printJavaVersionBuildSrc
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
> Task :printGroovyVersionBuildSrc
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
### Docker Alpine Test
* pull
```sh
docker pull gradle:5.4.1-jdk8-alpine
docker pull gradle:7.3.1-jdk17-alpine
docker pull gradle:8.1-jdk11-alpine
```
* repeat the following steps for all gradle releases
* build
```sh
IMAGE=basictask-gradle
docker build -t $IMAGE -f Dockerfile.alpine-jdk8-gradle .
```
* run (omit the `--rm` flag and optionally add the `--name` flag with argument to keep the container)
```sh
docker container run -u root --rm -it $IMAGE
```
repeat with each `Dockerfile.alpine-jdk11-gradle`,`Dockerfile.alpine-jdk11-gradle`,`Dockerfile.alpine-jdk11-gradle`

You will get on Gradle 5.x:
```text
Welcome to Gradle 5.4.1!

Here are the highlights of this release:
 - Run builds with JDK12
 - New API for Incremental Tasks
 - Updates to native projects, including Swift 5 support

For more details see https://docs.gradle.org/5.4.1/release-notes.html

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

> Task :printGroovyVersionBuildSrc
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

> Task :printJavaVersionBuildSrc FAILED

FAILURE: Build failed with an exception.

* What went wrong: A problem was found with the configuration of task ':printJavaVersionBuildSrc'.
> No value has been specified for property 'commandline'.

* Try:
Run with --stacktrace option to get the stack trace. Run with --info or --debug option to get more log output. Run with --scan to get full insights.

* Get more help at https://help.gradle.org

BUILD FAILED in 12m 46s
4 actionable tasks: 3 executed, 1 up-to-date

```


You will get on Gradle 7.x:
```text
Welcome to Gradle 7.3.1!

Here are the highlights of this release:
 - Easily declare new test suites in Java projects
 - Support for Java 17
 - Support for Scala 3

For more details see https://docs.gradle.org/7.3.1/release-notes.html

Starting a Gradle Daemon (subsequent builds will be faster)
> Task :buildSrc:compileJava
d^H^H> Task :buildSrc:compileGroovy
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

> Task :printGroovyVersionBuildSrc
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

BUILD SUCCESSFUL in 8m
7 actionable tasks: 6 executed, 1 up-to-date

```


You will get on Gradle 8.x:
```text

Welcome to Gradle 8.1.1!

Here are the highlights of this release:
 - Stable configuration cache
 - Experimental Kotlin DSL assignment syntax
 - Building with Java 20

For more details see https://docs.gradle.org/8.1.1/release-notes.html

Starting a Gradle Daemon (subsequent builds will be faster)
> Task :buildSrc:compileJava
> Task :buildSrc:compileGroovy
> Task :buildSrc:processResources
> Task :buildSrc:classes
> Task :buildSrc:jar
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

> Task :printGroovyVersionBuildSrc
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

BUILD SUCCESSFUL in 15m 5s
7 actionable tasks: 6 executed, 1 up-to-date
```

To review the updated files

```sh
NAME=basictask-gradle
docker container run --name $NAME -u root -it $IMAGE sh
```
```sh
gradle clean build printGroovyVersionBuildSrc printJavaVersionBuildSrc
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

* NOTE: one may like to change the Docker machine to have two CPU when testing the gradle tasks

![Docker Machine](https://github.com/sergueik/springboot_study/blob/master/basic-buildtask-gradle/screenshots/capture-docker-machine.png)

### Troubleshooting
* NOTE: if troublehooting, run shell
```sh
docker container run -u root -it $IMAGE sh
```
in the container

```sh
gradle clean build printJavaVersionBuildSrc
```
or 
```sh
gradle clean build printGroovyVersionBuildSrc
```
### Cleanup
```sh
docker image prune -f 
docker volume prune -f
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
