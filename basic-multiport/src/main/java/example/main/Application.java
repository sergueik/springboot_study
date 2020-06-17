package example.main;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Import;

import example.config.EmbeddedTomcatConfiguration;
import example.config.FiltersConfiguration;
import example.config.JavaMelodyConfiguration;

@SpringBootApplication(scanBasePackages = { "example.rest" })
@Import({ EmbeddedTomcatConfiguration.class, JavaMelodyConfiguration.class, FiltersConfiguration.class })
public class Application {
	public static void main(String[] args) {
		SpringApplication.run(Application.class, args);
	}
}
