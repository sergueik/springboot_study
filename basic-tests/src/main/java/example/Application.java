package example;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.ComponentScan;

// NOTE: SpringBootApplication cannot be default package

@ComponentScan(basePackages = { "example.component", "example.model", "example.service", "example.controller" })

@SpringBootApplication
public class Application {

	public static void main(String[] args) {
		SpringApplication.run(Application.class, args);
	}
}
