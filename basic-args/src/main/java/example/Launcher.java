package example;

import java.util.ArrayList;
import java.util.List;

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
public class Launcher implements ApplicationRunner {

	private static final Logger logger = LoggerFactory.getLogger(Launcher.class);

	public static void main(String[] args) {
		SpringApplication.run(Launcher.class, args);
	}

	@Autowired
	private Params params;

	@Autowired
	private Application app;

	@Override
	public void run(ApplicationArguments args) throws Exception {
		// Cannot instantiate the type ApplicationArguments
		// Provides access to the arguments that were used to run a
		// SpringApplication
		for (String name : args.getOptionNames()) {
			logger.info("arg-" + name + "=" + args.getOptionValues(name));
		}
		// NOTE: not reached when run through maven spring-boot:run
		// logger.info(String.format("Params passed by MAIN: %d", params.getId()));

		if (args.getOptionValues("params") != null) {
			logger.info("Defined params: " + args.getOptionValues("params").get(0));
		}

		app.logConfiguration();
	}
}
