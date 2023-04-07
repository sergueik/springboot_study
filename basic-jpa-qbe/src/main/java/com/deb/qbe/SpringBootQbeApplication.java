package com.deb.qbe;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;

@Configuration
@ComponentScan(basePackages = {"com.deb.qbe.*"})
@EnableJpaRepositories
@SpringBootApplication
public class SpringBootQbeApplication {

	public static void main(String[] args) {
		SpringApplication.run(SpringBootQbeApplication.class, args);
	}

}
