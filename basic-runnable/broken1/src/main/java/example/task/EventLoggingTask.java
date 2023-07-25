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

	// NOTE: uncommenting the following code leads to task never run
	@Autowired
	private Properties properties;

	// TODO: public EventLoggingTask(@Autowired ReloadableProperties properties)
	/*
	public EventLoggingTask(@Autowired Properties properties) {
		this.properties = properties;
	}
	*/

	@Override
	public void run() {

		logger.info("Value #1 is {}", value1);
		try {
			final String value2 = properties.getProperty("setting.value");
			logger.info("Value #2 are {} {}", value2);
		} catch (Exception e) {
			logger.info("Exception " + e.toString());
		}

	}
}
