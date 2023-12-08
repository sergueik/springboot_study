package example;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;


@SpringBootApplication
// NOTE: problem with Spring Boot 3:
//  package org.springframework.context.properties does not exist
// see also: https://www.baeldung.com/spring-enable-config-properties
// https://stackoverflow.com/questions/75044304/custom-spring-boot-3-starter-does-not-create-configurationproperties-beans
public class Application {

	public static void main(String[] args) {
		SpringApplication.run(Application.class, args);
	}

}
