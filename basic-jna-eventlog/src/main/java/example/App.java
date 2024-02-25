package example;

import java.io.Serializable;

import org.slf4j.LoggerFactory;

import ch.qos.logback.classic.Logger;

public class App {

	static Logger logger = (Logger) LoggerFactory.getLogger("mapAppender");

	public static void main(String[] args) {

		StringBuilder b = new StringBuilder();
		for (String str : args) {
			b.append(str);
			b.append(' ');
		}
		String message = b.toString();
		logger.warn("Event log from {} message {}", App.class.getSimpleName(),
				message);
	}
}
