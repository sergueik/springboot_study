package example;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.env.PropertySource;
import org.springframework.core.env.SimpleCommandLinePropertySource;

@Configuration
public class CommandLineConfiguration {
	private static final Logger logger = LoggerFactory
			.getLogger(CommandLineConfiguration.class);
	// every annotated recognized autowired parameter must be listed here
	@Value("${appname}")
	private String appname;
	@Value("${status}")
	private Boolean status;

	public CommandLineConfiguration() {
	}
}
