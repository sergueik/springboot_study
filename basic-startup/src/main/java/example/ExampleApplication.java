package example;

import java.io.InputStream;
import java.util.Arrays;
import java.util.Map;

import org.yaml.snakeyaml.Yaml;

import javax.annotation.PostConstruct;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.core.env.Environment;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

// NOTE: SpringBootApplication cannot be put in default package
@SpringBootApplication
// @EnableConfigurationProperties(ApplicationProperties.class)
@RestController

@RequestMapping("/basic")
public class ExampleApplication {

	private static final Logger logger = LoggerFactory
			.getLogger(ExampleApplication.class);

	@Autowired
	private Environment environment;

	@Autowired
	private ApplicationProperties properties;

	public static void main(String[] args) {
		SpringApplication.run(ExampleApplication.class, args);
	}

	@GetMapping
	public String hello() {
		return "Hello " + properties.getArtistName();
	}

	@PostConstruct
	public void init() {
		Yaml yaml = new Yaml();
		InputStream inputStream = this.getClass().getClassLoader()
				.getResourceAsStream("configuration.yaml");
		Map<String, Object> configuration = yaml.load(inputStream);
		logger.info("Loaded Env Default Profiles: {}",
				Arrays.asList(environment.getDefaultProfiles()));
		logger.info("Loaded lean configuration: application.artist_name: {}",
				properties.getArtistName());
		// TODO: second argument does not work

		logger.info("Loaded configuration: {}", configuration);
	}

}
