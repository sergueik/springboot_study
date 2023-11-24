package example.task;

import java.util.Map;
import java.util.Properties;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

@Component
@Scope("prototype")
public class EventLoggingTask implements Runnable {
	private Logger logger = LoggerFactory.getLogger(EventLoggingTask.class);

	@Value("${setting.value:101}")

	private long value1;
	@Value("${spring.profile.active:development}")
	// NOTE:
	// Injection of autowired dependencies failed; nested exception is
	// java.lang.IllegalArgumentException:
	// Could not resolve placeholder 'spring.profile.active' in value
	// "${spring.profile.active}"
	private String profile;

	public Properties properties;

	@Value("#{${example.osspecific.application.path}}")
	private Map<String, String> applicationPaths;

	@Value("#{${example.osspecific.expand}}")
	private Map<String, String> expandEnvVars;

	private static String osName = getOSName();

	public static String getOSName() {
		String osName = System.getProperty("os.name").toLowerCase();
		return (osName.startsWith("windows"))
				? (System.getProperty("os.arch").contains("64")) ? "windows x64"
						: "windows"
				: "linux";
	}

	public static String resolveEnvVars(String input) {
		if (null == input) {
			return null;
		}
		Pattern p = Pattern.compile("\\$(?:\\{(?:env:)?(\\w+)\\}|(\\w+))");
		Matcher m = p.matcher(input);
		StringBuffer sb = new StringBuffer();
		while (m.find()) {
			String envVarName = null == m.group(1) ? m.group(2) : m.group(1);
			String envVarValue = System.getenv(envVarName);
			m.appendReplacement(sb,
					null == envVarValue ? "" : envVarValue.replace("\\", "\\\\"));
		}
		m.appendTail(sb);
		return sb.toString();
	}

	@Override
	public void run() {
		logger.info("os.arch = {}", System.getProperty("os.arch"));
		String applicationPath = applicationPaths.get(osName);
		String expandEnvVar = resolveEnvVars(expandEnvVars.get(osName));
		logger.info(
				"Run with value1 = {}, profile = {}, applicationPath = {}, expandEnvVar = {} through annotation",
				value1, profile, applicationPath, expandEnvVar);
		try {
			properties = new Properties();
			properties
					.load(this.getClass().getResourceAsStream("/application.properties"));
			final String value2 = properties.getProperty("setting.value", "12345");
			// NOTE: will need to load Map<String, String> from string manually
			// final Map<String, String> data =
			// properties.getProperty("example.osspecific.application.path");
			logger.info(
					"Read confguration from resource \"{}\" within application: value1 = {}",
					"/application.properties", value2);
			// alternatively feed the properties object from the file or even read
			// properties map directly from properties or YAML
			// see also:
			// basic-properties/src/main/java/example/component/ExplicitPropertiesParser.java
		} catch (Exception e) {
			logger.info("Exception (ignored): " + e.toString());
		}

	}
}
