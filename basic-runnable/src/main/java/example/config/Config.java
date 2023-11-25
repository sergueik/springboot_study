package example.config;

import java.util.Arrays;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

import example.utils.Utils;

@Component
@Scope("prototype")
public class Config {

	private Logger logger = LoggerFactory.getLogger(Config.class);

	// using singleton for config leads to NPE
	// private static Config instance = new Config();

	private static String osName = null;
	private boolean debug = true;

	@Value("${setting.value:101}")
	private long value;

	@Value("#{${example.osspecific.application.path}}")
	private Map<String, String> applicationPaths;

	@Value("#{${example.osspecific.expand}}")
	private Map<String, String> expandEnvVars;

	private String applicationPath;
	private String expandEnvVar;

	@Value("${spring.profile.active:development}")
	// NOTE:
	// Injection of autowired dependencies failed; nested exception is
	// java.lang.IllegalArgumentException:
	// Could not resolve placeholder 'spring.profile.active' in value
	// "${spring.profile.active}"
	private String profile;

	public long getValue() {
		return value;
	}

	public String getProfile() {
		return profile;
	}

	public String getApplicationPath() {
		if (debug)
			logger.info("Config: applicationPaths keys: {}",
					Arrays.asList(applicationPaths.keySet()));
		applicationPath = applicationPaths.get(osName);
		return applicationPath;
	}

	public String getExpandEnvVar() {
		if (debug)
			logger.info("Config:expandEnvVars keys: {}",
					Arrays.asList(expandEnvVars.keySet()));
		expandEnvVar = Utils.resolveEnvVars(expandEnvVars.get(osName));
		return expandEnvVar;
	}

	public Config() {
		osName = Utils.getOSName();
		if (debug)
			logger.info("Config: os name: {}", osName);
	}

	public void setDebug(boolean value) {
		debug = value;
	}

}
