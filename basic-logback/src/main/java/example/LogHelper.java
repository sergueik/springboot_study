package example;

import javax.annotation.PostConstruct;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.stereotype.Component;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

// origin: https://github.com/eugenp/tutorials/tree/master/logging-modules/logback/src/main/java/com/baeldung/logback
// and https://github.com/maul5/example-springboot-logback2

@Component
public class LogHelper {

	private static Logger logger = null;

	// org.springframework.beans.factory.UnsatisfiedDependencyException: Error
	// creating bean with name 'logHelper'
	// Unsatisfied dependency expressed through constructor parameter 0; nested
	// exception is
	// org.springframework.beans.factory.NoSuchBeanDefinitionException:
	// No qualifying bean of type 'java.lang.Class<?>' available:
	// expected at least 1 bean which qualifies as autowire candidate. Dependency
	// annotations: {}
	public LogHelper(/* Class consumerClass */ ) {
		logger = LoggerFactory.getLogger(LogHelper.class);
		logger.info("init message");
		logger.warn("init message");
		// DEBUG will not be shown as long as root level is WARN
		logger.debug("init message");
	}

	public void logAll(String data) {
		// these logs are configured to be available only in the file
		logger.info("{}", data);
		logger.warn("{}", data);
		logger.debug("{}", data);
	}
}
