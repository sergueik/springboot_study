package example;

import javax.annotation.PostConstruct;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

// origin: https://github.com/eugenp/tutorials/tree/master/logging-modules/logback/src/main/java/com/baeldung/logback
// and https://github.com/maul5/example-springboot-logback2

@SpringBootApplication
@RestController
@RequestMapping("/example")
public class Example {
	private static final Logger logger = LoggerFactory.getLogger(Example.class);

	@PostConstruct // init-method
	public void init() {
		logger.info("init message");
		logger.warn("init message");
		// DEBUG will not be shown as long as root level is WARN
		// NOTE: the Example.class.getSimpleName() will be added by transformer.
		// No need to add explicitly
		logger.debug("DEBUG {} init message", Example.class.getSimpleName());
	}

	public static void main(String[] args) {
		SpringApplication.run(Example.class, args);
		// can chain close
		// SpringApplication.run(Example.class, args).close();
	}

	// https://www.baeldung.com/spring-request-param
	@GetMapping
	public String exampleHandler(@RequestParam(required = false) String data) {
		// these logs are configured to be available only in the file
		logger.info("exampleHandler received: {}", data);
		logger.warn("exampleHandler received: {}", data);
		logger.debug("exampleHandler received: {}", data);
		return ("exampleHandler received: " + data);
	}
}
