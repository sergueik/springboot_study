package example;

import org.slf4j.Logger;

import org.slf4j.LoggerFactory;

// origin: https://github.com/eugenp/tutorials/tree/master/logging-modules/logback/src/main/java/com/baeldung/logback
//
public class Example {

	private static final Logger logger = LoggerFactory.getLogger(Example.class);

	public static void main(String[] args) {
		logger.info("Example INFO message from {}", Example.class.getSimpleName());
		logger.warn("Example WARN message from {}", Example.class.getSimpleName());
		logger.debug("Example DEBUG message from {}",
				Example.class.getSimpleName());
	}

}
