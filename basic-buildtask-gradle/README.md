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
```sh
docker run -u root --rm -it gradle:5.4.1-jdk8-alpine sh
```

### See Also
  * https://www.linkedin.com/pulse/writing-custom-gradle-plugin-using-java-mahendra-tonape/
  * [gretl plugin](https://github.com/sogis/gretl)
  * https://github.com/gpetuhov/SampleGradle
DefaultTask  * example [Multi Module Gradle project with Custom Gradle Plugin and Custom task](https://github.com/TechPrimers/gradle-custom-plugin-example-2)
  * https://docs.gradle.org/current/userguide/more_about_tasks.html
  * https://docs.gradle.org/current/userguide/custom_tasks.html#sec:writing_a_simple_task_class

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
