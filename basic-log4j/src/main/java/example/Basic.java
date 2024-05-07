package example;

// NOTE: switched to log4j2

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class Basic {
	private static final Logger LOGGER = LogManager.getLogger(Basic.class);

	public static void main(String a[]) {
		LOGGER.debug("Debug message");
		LOGGER.info("Info message");
	}
}
