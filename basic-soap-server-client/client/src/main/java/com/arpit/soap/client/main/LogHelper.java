package com.arpit.soap.client.main;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.Level;
import org.springframework.stereotype.Component;

@Component
public class LogHelper {

	private static final Logger logger = LogManager.getLogger(LogHelper.class);

	public static Logger getLogger() {
		return logger;
	}

	public LogHelper(/* Class consumerClass */ ) {
		logger.info("init message");
		logger.warn("init message");
		logger.debug("init message");
	}

	public void info(String data) {
		logger.info("{}", data);
	}

	public void trace(String data) {
		logger.trace("{}", data);
	}

	public void warn(String data) {
		logger.warn("{}", data);
	}

	public void debug(String data) {
		logger.debug("{}", data);
	}

	public void logAll(String data) {
		logger.info("{}", data);
		logger.warn("{}", data);
		logger.debug("{}", data);
	}
}
