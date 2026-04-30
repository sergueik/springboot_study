### Info

Replica of [danvega/spring-io-mcp](https://github.com/danvega/spring-io-mcp)
a MCP server implemnting RPC style on __Spring boot__ spring-boot-starter-parent [3.5](https://mvnrepository.com/artifact/org.springframework.boot/spring-boot-starter-parent/3.5.0) - using ``
For `streamable  HTTP` see [danvega/dvaas](https://github.com/danvega/dvaas) - used later relase  of __Spring Boot__ spring-boot-starter-parent [3.5.5](https://mvnrepository.com/artifact/org.springframework.boot/spring-boot-starter-parent/3.5.5)

### Usage


build on JDK __17__ (the __24__ is overkill) 
```sh
curl -skLo ~/Downloads/apache-maven-3.8.9.zip https://dlcdn.apache.org/maven/maven-3/3.8.9/binaries/apache-maven-3.8.9-bin.zip
```
```sh
unzip -C /c/java ~/Downloads/apache-maven-3.8.9.zip 
```
```sh
set java_home=c:\java\jdk-17.0.12
c:\java\init.cmd
```

```sh
mvn -DskipTests package
```

### ToolChain Dependnecies

purely idiosyncratic:

* The plugin `maven-compiler-plugin`:__3.14.0__ requires Maven version __3.6.3__ 
* The plugin `org.apache.maven.plugins:maven-surefire-plugin`:__3.5.3__ requires Maven version __3.6.3__ 

### See Also 
  * [Model Context Protocol (MCP) with Spring AI in Minutes](https://www.youtube.com/watch?v=MarSC2dFA9g)
  * [avaialble MCP SDKs](https://modelcontextprotocol.io/docs/sdk#available-sdks)
  * [MCP spec](https://modelcontextprotocol.io/specification/2025-06-18)
  * [MCP basic](https://habr.com/ru/articles/960538/)( in Russian )

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
