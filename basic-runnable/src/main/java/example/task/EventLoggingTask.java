package example.task;

import java.util.Properties;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
public class EventLoggingTask implements Runnable {
	private Logger logger = LoggerFactory.getLogger(EventLoggingTask.class);

	@Value("${setting.value:101}")
	private long value1;

	public Properties properties;

	@Override
	public void run() {

		logger.info("Attempt to load value1 through annotation: {}", value1);
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
