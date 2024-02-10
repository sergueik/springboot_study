package example;

import java.io.Serializable;

public class App {

	public static void main(String[] args) {

		StringBuilder b = new StringBuilder();
		for (String str : args) {
			b.append(str);
			b.append(' ');
		}
		String message = b.toString();

		String name = "EventLog";

		final String server = "."; // guess
		final String source = "example.log4jna_sample";
		final String application = "log4jna_sample";
		final String eventMessageFile = "src\\main\\resources\\Win32EventLogAppender.dll";
		final String categoryMessageFile = "src\\main\\resources\\Win32EventLogAppender.dll";
		Win32EventLogAppender appender = Win32EventLogAppender.createAppender(name,
				server, source, application, eventMessageFile, categoryMessageFile);
		appender.append(message);
	}
}
