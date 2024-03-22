package example;

// NOTE: do not initialize specific logger
// import ch.qos.logback.classic.Logger;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class App {
	// NOTE: do not initialize specific logger
	// static Logger logger = (Logger) LoggerFactory.getLogger("mapAppender");
	static final Logger logger = LoggerFactory.getLogger(App.class);

	public static void main(String[] args) {

		StringBuilder b = new StringBuilder();
		for (String str : args) {
			b.append(str);
			b.append(' ');
		}
		String message = b.toString();
		logger.warn("Event log from {} message {}", App.class.getSimpleName(), message);
	}
}
