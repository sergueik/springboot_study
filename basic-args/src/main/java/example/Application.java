package example;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@Component
@RestController
@RequestMapping("/basic")
public class Application {
	@Value("${appname}")
	private String appname;
	@Value("${status}")
	private Boolean status;

	private static final Logger logger = LoggerFactory
			.getLogger(CommandLineConfiguration.class);

	public void logConfiguration() {
		logger.info("Loaded with appname: " + appname);
	}

	@GetMapping
	public String Hello() {
		return "This is " + appname + " The status is: " + status;
	}
}
