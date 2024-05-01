package example;

//NOTE: do not import "logback.classic" to initialize specific logger
//import ch.qos.logback.classic.Logger;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

// origin: https://github.com/eugenp/tutorials/tree/master/logging-modules/logback/src/main/java/com/baeldung/logback
// and https://github.com/maul5/example-springboot-logback2

@Component
public class LogHelper {

	// NOTE: for demonstration initialize custom logger
	static Logger eventlogAppenderlogger = (Logger) LoggerFactory.getLogger("eventlogAppender");
	private static Logger logger = null;

	// org.springframework.beans.factory.UnsatisfiedDependencyException: Error
	// creating bean with name 'logHelper'
	// Unsatisfied dependency expressed through constructor parameter 0; nested
	// exception is
	// org.springframework.beans.factory.NoSuchBeanDefinitionException:
	// No qualifying bean of type 'java.lang.Class<?>' available:
	// expected at least 1 bean which qualifies as autowire candidate.
	// Dependency
	// annotations: {}
	public LogHelper() {
		logger = LoggerFactory.getLogger(LogHelper.class);
		logger.info("logger constructor message");
		logger.warn("logger constructor message");
		// NOTE: DEBUG messages will not be shown as long as root level is WARN
		logger.debug("logger constructor message");
	}

	public LogHelper(Class<?> consumerClass) {
		logger = LoggerFactory.getLogger(consumerClass);
	}

	// NOTE: currently logging is exclusively in the file
	public void logAll(String message) {
		logger.error("message: {}", message);
		logger.warn("message: {}", message);
		logger.info("message: {}", message);
		logger.debug("message: {}", message);
		eventlogAppenderlogger.warn("Event log from {} {} message {}",
				eventlogAppenderlogger.getClass().getName(), Application.class.getName(), message);

	}
}
