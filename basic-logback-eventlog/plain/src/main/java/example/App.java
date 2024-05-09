package example;

// NOTE: do not import "logback.classic" to initialize specific logger
// import ch.qos.logback.classic.Logger;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class App {
	// NOTE: for demonstration initialize custom logger
	static Logger eventlogAppenderlogger = (Logger) LoggerFactory.getLogger("eventlogAppender");
	static final Logger logger = LoggerFactory.getLogger(App.class);

	public static void main(String[] args) {

		StringBuilder stringBuilder = new StringBuilder();
		for (String str : args) {
			stringBuilder.append(str);
			stringBuilder.append(' ');
		}
		final String message = stringBuilder.toString();
		logger.error(message);
		logger.warn(message);
		logger.info(message);
		logger.debug(message);
		eventlogAppenderlogger.warn("Event log directly through {} {} message {}", eventlogAppenderlogger.getClass().getName(),
				App.class.getName(), message);
	}
}
