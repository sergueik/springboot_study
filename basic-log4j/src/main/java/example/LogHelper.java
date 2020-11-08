package example;

import org.apache.log4j.Logger;
import org.springframework.stereotype.Component;

@Component
public class LogHelper {

	private static final Logger logger = Logger.getLogger(LogHelper.class);

	public static Logger getLogger() {
		return logger;
	}

	public LogHelper(/* Class consumerClass */ ) {
		logger.info("init message");
		logger.warn("init message");
		logger.debug("init message");
	}

	public void info(String data) {
		logger.info("INFO: " + data);
	}

	public void trace(String data) {
		logger.trace("TRACE: "+ data);
	}

	public void warn(String data) {
		logger.warn("WARN: " + data);
	}

	public void debug(String data) {
		logger.debug("DEBUG: " + data);
	}

	public void logAll(String data) {
		info(data);
		warn(data);
		debug(data);
	}
}
