package example;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

// NOTE: will not be posssible to import two different Logger classes -
// The import ch.qos.logback.classic.Logger collides with another import statement
// will need to s[ecofu ull declaration
// import ch.qos.logback.classic.Logger;

public class App {

	private static final Logger logger = LoggerFactory.getLogger(App.class);
	static ch.qos.logback.classic.Logger specialLogger = (ch.qos.logback.classic.Logger) LoggerFactory
			.getLogger("mapAppender");

	public static void main(String[] args) {
		logger.info("Example log from {}", App.class.getSimpleName());

		specialLogger.warn("Special Example log from {}",
				App.class.getSimpleName());
	}

}
