package example;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.core.env.PropertySource;
import org.springframework.core.env.SimpleCommandLinePropertySource;

@SpringBootApplication
public class ExampleApplication implements ApplicationRunner {

	private static final Logger logger = LoggerFactory
			.getLogger(ExampleApplication.class);

	public static void main(String[] args) {
		SpringApplication.run(ExampleApplication.class, args);
	}

	@Value("${appname}")
	private String appname;

	@Autowired
	private BasicApplication comp;

	@Override
	public void run(ApplicationArguments args) throws Exception {
		for (String name : args.getOptionNames()) {
			logger.info("arg-" + name + "=" + args.getOptionValues(name));
		}
		logger.info(String.format("Name passed by MAIN: %s", appname));

		if (args.getOptionValues("appname") != null) {
			// String nameOption = args.getOptionValues("name").get(0);
			logger.info("Defined name: " + args.getOptionValues("appname").get(0));
		}

		comp.logConfiguration();
	}
}
