package example;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.stereotype.Component;

@Component
public class LogHelper {

	private static final Logger logger = LogManager.getLogger(LogHelper.class);

	public LogHelper(/* Class consumerClass */ ) {
		logger.info("init message");
		logger.warn("init message");
		// debug() will not be logged when root level is WARN
		logger.debug("init message");
	}

	public void info(String data) {
		logger.info("{}", data);
	}

	public void warn(String data) {
		logger.warn("{}", data);
	}

	public void debug(String data) {
		logger.debug("{}", data);
	}

	public void logAll(String data) {
		// these logs are configured to be available only in the file
		logger.info("{}", data);
		logger.warn("{}", data);
		logger.debug("{}", data);
	}
}
