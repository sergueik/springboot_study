package example.config;

import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

import example.task.EventLoggingTask;
import example.utils.Utils;

import java.util.Arrays;
import java.util.Map;
import java.util.Properties;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

@Component
@Scope("prototype")
public class Config {

	private Logger logger = LoggerFactory.getLogger(Config.class);

	// using singleton for config leads to NPE
	// private static Config instance = new Config();

	private static String osName = null;
	private boolean debug = false;

	@Value("${setting.value:101}")
	private long value1;

	@Value("${spring.profile.active:development}")
	// NOTE:
	// Injection of autowired dependencies failed; nested exception is
	// java.lang.IllegalArgumentException:
	// Could not resolve placeholder 'spring.profile.active' in value
	// "${spring.profile.active}"
	private String profile;

	public long getValue1() {
		return value1;
	}

	public String getProfile() {
		return profile;
	}

	public Properties properties;

	@Value("#{${example.osspecific.application.path}}")
	private Map<String, String> applicationPaths;

	@Value("#{${example.osspecific.expand}}")
	private Map<String, String> expandEnvVars;
	private String applicationPath;
	private String expandEnvVar;

	public String getApplicationPath() {
		return applicationPath;
	}

	public String getExpandEnvVar() {
		return expandEnvVar;
	}

	/* private */ public Config() {
		logger.info("Config calling Utils");
		osName = Utils.getOSName();
		logger.info("Config: os name: {}", osName);
		logger.info("Config: applicationPaths keys: {}",
				Arrays.asList(applicationPaths.keySet()));
		applicationPath = applicationPaths.get(osName);
		expandEnvVar = Utils.resolveEnvVars(expandEnvVars.get(osName));
		logger.info(
				"Run with value1 = {}, profile = {}, applicationPath = {}, expandEnvVar = {} through annotation",
				value1, profile, applicationPath, expandEnvVar);
	}

	// public static Config getInstance() {
	// return instance;
	// }

	public void setDebug(boolean value) {
		debug = value;
	}

}
