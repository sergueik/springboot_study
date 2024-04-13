package example;

/**
 * Copyright 2023,2024 Serguei Kouzmine
 */

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
		final String source = "Application Error"; // = "example.log4jna_sample";
		final String application  = "Application"; // = "log4jna_sample";
		final String eventMessageFile = "%SystemRoot%\\Microsoft.NET\\Framework\\v4.0.30319\\EventLogMessages.dll";
		final String categoryMessageFile = "%SystemRoot%\\System32\\wer.dll"; // "%SystemRoot%\\system32\\wevtapi.dll"; // "%SystemRoot%\\Microsoft.NET\\Framework\\v4.0.30319\\EventLogMessages.dll";
		Win32EventLogAppender appender = Win32EventLogAppender.createAppender(name,
				server, source, application, eventMessageFile, categoryMessageFile);
		appender.append(message);
	}
}
