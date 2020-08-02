package example;

import javax.annotation.PostConstruct;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
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
	@Autowired
	LogHelper loghelper;

	public static void main(String[] args) {
		SpringApplication.run(Example.class, args);
		// can chain close
		// SpringApplication.run(Example.class, args).close();
	}

	// https://www.baeldung.com/spring-request-param
	@GetMapping
	public String exampleHandler(@RequestParam(required = false) String data) {
		// these logs are configured to be available only in the file

		loghelper.logAll(String.format("exampleHandler received: %s", data));
		return ("exampleHandler received: " + data);
	}
}
