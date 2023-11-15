package example.task;

import java.util.Properties;

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

	@Override
	public void run() {

		logger.info("Run with value1={},profile={} through annotation", value1,
				profile);
		try {
			properties = new Properties();
			properties
					.load(this.getClass().getResourceAsStream("/application.properties"));
			final String value2 = properties.getProperty("setting.value", "12345");
			logger.info("Read  value2 is from resource \"{}\" within application: {}",
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
