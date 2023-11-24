package example.task;

import java.util.Map;
import java.util.Properties;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

import example.config.Config;
import example.utils.Utils;

@Component
// @Scope("prototype")
public class EventLoggingTask implements Runnable {
	private Logger logger = LoggerFactory.getLogger(EventLoggingTask.class);

	@Autowired
	private Config config;

	// private Config config = new Config(); // Config.getInstance();
	private String applicationPath = config.getApplicationPath();
	private String expandEnvVar = config.getExpandEnvVar();

	private long value1 = config.getValue1();
	private String profile = config.getProfile();

	@Override
	public void run() {

		logger.info(
				"Run with value1 = {}, profile = {}, applicationPath = {}, expandEnvVar = {} through annotation",
				value1, profile, applicationPath, expandEnvVar);
	}
}
