package example.task;
/**
 * Copyright 2024 Serguei Kouzmine
 */

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

// NOTE: do not import "logback.classic" to initialize specific logger
// import ch.qos.logback.classic.Logger;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import example.config.Config;

@Component
// NOTE: "scope" annotation is optional here
@Scope("prototype")
public class EventLoggingTask implements Runnable {
	// NOTE: for demonstration initialize custom logger
	static Logger eventlogAppenderlogger = (Logger) LoggerFactory.getLogger("eventlogAppender");
	private Logger logger = LoggerFactory.getLogger(EventLoggingTask.class);

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
	public void run() {
		applicationPath = config.getApplicationPath();
		expandEnvVar = config.getExpandEnvVar();

		value = config.getValue();
		profile = config.getProfile();
		applicationOsSpecificPath = config.getApplicationOsSpecificPath();
		logger.info(
				"Run with value = {}, profile = {}, applicationPath = {}, expandEnvVar = {}, applicationOsSpecificPath = {} through annotation",
				value, profile, applicationPath, expandEnvVar, applicationOsSpecificPath);
		eventlogAppenderlogger.info(
				"Logger {} Run with value = {}, profile = {}, applicationPath = {}, expandEnvVar = {}, applicationOsSpecificPath = {} through annotation",
				eventlogAppenderlogger.getClass().getName(),  value, profile, applicationPath, expandEnvVar, applicationOsSpecificPath);

	}
}
