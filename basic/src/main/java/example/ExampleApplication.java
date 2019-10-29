package example;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

// TODO: does not see its own project tree (does not need to with Spring though)
// import example.resource.BasicResource;

// NOTE: SpringBootApplication cannot be default package
@SpringBootApplication
public class ExampleApplication {

	public static void main(String[] args) {
		SpringApplication.run(ExampleApplication.class, args);
	}
}
