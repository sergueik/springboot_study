# OpenAPI Example
This project shows how to use the OpenAPI specification to define a RESTful web service.


## How to read the instructions?
**Module Name** : Instruction.

## Instructions
1. **OpenAPI-example :** Create new spring-boot project.
2. **project-api :** Add new yaml file library-api.yaml which contains OpenAPI specification.
3. **project-api :** Add below dependency for Swagger code generation.

```xml
   <!-- For swagger codegen begin -->

   <dependency>
   	<groupId>io.swagger</groupId>
   	<artifactId>swagger-models</artifactId>
   	<version>1.6.0</version>
   </dependency>
   <dependency>
   	<groupId>com.fasterxml.jackson.core</groupId>
   	<artifactId>jackson-annotations</artifactId>
   	<version>2.12.6</version>
   </dependency>
   <dependency>
   	<groupId>javax.validation</groupId>
   	<artifactId>validation-api</artifactId>
   </dependency>

   <!-- For swagger codegen end -->
```

4. **project-api :** Add below build plugin for Swagger code generation.
```xml
   <!-- Plugin for generating stub from openapi specification begin -->
   <plugin>
   	<groupId>io.swagger.codegen.v3</groupId>
   	<artifactId>swagger-codegen-maven-plugin</artifactId>
   	<version>3.0.18</version>
   	<executions>
   		<execution>
   			<goals>
   				<goal>generate</goal>
   			</goals>
   			<configuration>
   				<inputSpec>${project.basedir}/src/main/resources/library-api.yaml</inputSpec>
   				<language>spring</language>
   				<output>${project.build.directory}/generated-sources/</output>
   				<generateSupportingFiles>false</generateSupportingFiles>
   				<apiPackage>com.example.demo.api</apiPackage>
   				<modelPackage>com.example.demo.models</modelPackage>
   				<configOptions>
   					<interfaceOnly>true</interfaceOnly>
   				</configOptions>
   			</configuration>
   		</execution>
   	</executions>
   </plugin>
   <!-- Plugin for generating stub from openapi specification end -->
```

5. **OpenAPI-example :** Execute `mvn clean install`, which generates api and model classes in the target folder of `project-api`.

6. **project-service :** Create new rest-controller `BookController` which implements generated interface `BooksApi`
