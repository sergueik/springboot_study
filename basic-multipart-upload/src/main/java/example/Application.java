package example;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.context.properties.EnableConfigurationProperties;

import example.property.FileStorageProperties;

// NOTE: SpringBootApplication cannot be default package

@SpringBootApplication
@EnableConfigurationProperties({ FileStorageProperties.class })
public class Application {

	public static void main(String[] args) {
		SpringApplication.run(Application.class, args);
	}
}
