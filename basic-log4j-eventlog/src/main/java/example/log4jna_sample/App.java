package example.log4jna_sample;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class App {

	private static final Logger log = LogManager.getLogger(App.class);

	public static void main(String[] args) {

		StringBuilder b = new StringBuilder();
		for (String str : args) {
			b.append(str);
			b.append(' ');
		}
		String message = b.toString();

		log.fatal(message);
		log.error(message);
		log.warn(message);
		log.info(message);
		log.debug(message);
		log.trace(message);

	}
}
