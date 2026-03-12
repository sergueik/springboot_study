package example;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Service {

	private static final Logger logger = LoggerFactory.getLogger(Service.class);

	public void doWorkVerbosely() {
		logger.info("starting work");
	}

	public void doWorkQuietly() {
	}
}
