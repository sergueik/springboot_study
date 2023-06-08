### Info

trimmed replica of
[https://www.baeldung.com/gradle-custom-task](https://www.baeldung.com/gradle-custom-task) example

### Usage

```sh
gradle clean build printJavaVersionBuildSrc
```

```text
 gradle clean build printJavaVersionBuildSrc

> Task :printJavaVersionBuildSrc
1.8.0_161

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
