package example;

import java.util.Properties;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;

@SpringBootApplication
@ComponentScan(basePackages = { "example" })
@EnableJpaRepositories(basePackages = "example.repository")
public class Launcher {

	public static Properties prop = new Properties();

	public static void main(String[] args) throws Exception {
		SpringApplication.run(Launcher.class, args);
	}
}
