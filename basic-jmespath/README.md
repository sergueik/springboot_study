### Info
this directory contains basic skeleton project to exercise testing the [jmespath-java](https://github.com/burtcorp/jmespath-java)( the
implementation of the JMES Path JSON Query langauge foir Java)

### Usage

```sh
mvn dependency:copy-dependencies
```
```sh
mvn test
```
### NOTE

the "main* maven dependency `jmespath` dependency on [maven central](https://mvnrepository.com/artifact/io.burt/jmespath) is a `pom` type and one needs to select the adapter:
```XML
    <jmespath.version>0.2.0</jmespath.version>
    <jmespath.adapter>jmespath-jackson</jmespath.adapter>
  </properties>
  <dependencies>
<dependency>
  <groupId>io.burt</groupId>
  <artifactId>${jmespath.adapter}</artifactId>
  <version>${jmespath.version}</version>
</dependency>
```

### See Also
   * https://jmespath.org/tutorial.html
   * https://www.baeldung.com/jackson-json-to-jsonnode
   * https://github.com/NikiforovAll/jmespath-demo:q::
   * https://nikiforovall.github.io/dotnet/2023/01/08/jmespath-intro.html
   * https://github.com/burtcorp/jmespath-java
   * https://github.com/burtcorp/jmespath-java-contrib
   * https://github.com/leopold2410/xml_to_json
   * https://www.nuget.org/packages/JmesPath.Net (supports .Net Framework 4.5)
   * https://github.com/joao2391/Examples_Tools/blob/master/src/Jmespath/BasicExpressions.cs
   * https://github.com/NikiforovAll/jmespath-demo
   * https://github.com/jdevillard/JmesPath.Net/commits/master/?after=e59be9c0c0dfeea84f1bb65ddc1f776f063056c5+69
 -  there is alternatibe vendor, with very frequent [releases that are all published to maven cenral](https://mvnrepository.com/artifact/com.amazonaws/jmespath-java)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
