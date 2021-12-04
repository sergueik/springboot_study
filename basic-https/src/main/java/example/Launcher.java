package example;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.PropertySource;

@SpringBootApplication
@PropertySource("classpath:application.properties")
public class Launcher {

	public static void main(String[] args) {
		SpringApplication application = new SpringApplication(Launcher.class);
		application.setAdditionalProfiles("ssl");
		application.run(args);
	}
}
