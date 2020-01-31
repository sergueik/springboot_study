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
		/*
			List<String> fabArgs = new ArrayList<>();
		if (args.length > 0) {
			for (int cnt = 0; cnt != args.length; cnt++) {
				System.err.println("Arguments # " + cnt + ": " + args[cnt]);
				fabArgs.add(args[cnt]);
			}
			fabArgs.add("-Dstatus=true");
		}
		SpringApplication.run(Launcher.class,
				(String[]) fabArgs.toArray(new String[fabArgs.size()]));
				*/
		SpringApplication.run(Launcher.class, args);
	}

	/*
	 org.springframework.beans.factory.UnsatisfiedDependencyException: 
	 Error creating bean with name 'launcher': 
	 Unsatisfied dependency expressed through field 'app'; 
	 nested exception is org.springframework.beans.factory.BeanCreationException: 
	Error creating bean with name 'application': 
	Injection of autowired dependencies failed; 
	nested exception is java.lang.IllegalArgumentException: Could not resolve placeholder 'status' in value "${status}"
	 */

	// Unsatisfied dependency expressed through field 'result';
	// nested exception is org.springframework.beans.TypeMismatchException: Failed
	// to convert value of
	// type 'java.lang.String' to required type 'int'; nested exception is
	// java.lang.NumberFormatException: For input string: "___" -> [Help 1]
	@Value("${appname}")
	private String appname;
	@Value("${status}")
	private Boolean status = true;

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
		logger.info(String.format("Name passed by MAIN: %s", appname));

		if (args.getOptionValues("appname") != null) {
			logger.info("Defined appname: " + args.getOptionValues("appname").get(0));
		}

		app.logConfiguration();
	}
}
