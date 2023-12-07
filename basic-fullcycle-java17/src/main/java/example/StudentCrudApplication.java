package example;

import org.springframework.boot.SpringApplication;
//import org.springframework.context.properties.EnableConfigurationProperties;
import org.springframework.boot.autoconfigure.SpringBootApplication;

@SpringBootApplication
// NOTE: problem with Spring Boot 3:
//  package org.springframework.context.properties does not exist
// see also: https://www.baeldung.com/spring-enable-config-properties
// https://stackoverflow.com/questions/75044304/custom-spring-boot-3-starter-does-not-create-configurationproperties-beans
// @EnableConfigurationProperties({ FileStorageProperties.class })
public class StudentCrudApplication {

	public static void main(String[] args) {
		SpringApplication.run(StudentCrudApplication.class, args);
	}

}
