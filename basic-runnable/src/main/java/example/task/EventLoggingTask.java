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

	@Value("${setting.value:123}")
	private long value1;

	public Properties properties;

	@Override
	public void run() {

		logger.info("Value #1 is loaded through annotation: {}", value1);
		try {
			properties = new Properties();
			properties
					.load(this.getClass().getResourceAsStream("/application.properties"));
			final String value2 = properties.getProperty("setting.value", "123");
			logger.info("Value #2 is read from \"{}\": {}", "/application.properties",
					value2);
		} catch (Exception e) {
			logger.info("Exception (ignored): " + e.toString());
		}

	}
}
