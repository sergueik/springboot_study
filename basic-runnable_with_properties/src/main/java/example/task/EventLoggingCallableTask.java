package example.task;

import java.util.concurrent.Callable;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

import example.config.Config;

@Component
// NOTE: "scope" annotation is optional here
// https://www.baeldung.com/java-pattern-prototype
@Scope("prototype")
public class EventLoggingCallableTask implements Callable<String> {
	private Logger logger = LoggerFactory.getLogger(EventLoggingCallableTask.class);

	@Autowired
	private Config config;

	// NOTE: constructing the Config instance directly leads to
	// NPE in the Config class loading its properties
	// private Config config = new Config();

	// NOTE: early initialization leads to NPE
	// private String applicationPath = config.getApplicationPath();
	private String applicationPath = null;
	private String applicationOsSpecificPath = null;
	private String expandEnvVar = null;
	private long value = 0L;
	private String profile = null;

	@Override
	public String call() {
		applicationPath = config.getApplicationPath();
		expandEnvVar = config.getExpandEnvVar();

		value = config.getValue();
		profile = config.getProfile();
		applicationOsSpecificPath = config.getApplicationOsSpecificPath();
		String message = String.format(
				"Run with value = %d, profile = %s, applicationPath = %s, expandEnvVar = %s, applicationOsSpecificPath = %s through annotation",
				value, profile, applicationPath, expandEnvVar, applicationOsSpecificPath);
		logger.info(message);
		return message;

	}
}
