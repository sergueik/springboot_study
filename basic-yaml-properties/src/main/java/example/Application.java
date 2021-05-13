package example;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

import example.config.GlobalProperties;
import example.config.ApplicationProperties;

@SpringBootApplication
public class Application implements CommandLineRunner {

	@Autowired
	private ApplicationProperties applicationProperties;

	@Autowired
	private GlobalProperties globalProperties;

	public static void main(String[] args) {
		SpringApplication.run(Application.class, args);
	}

	@Override
	public void run(String... args) {
		System.out.println(globalProperties);
		System.out.println(applicationProperties);
	}
}
