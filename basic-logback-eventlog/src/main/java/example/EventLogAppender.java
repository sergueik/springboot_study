package example;

import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.AppenderBase;

import java.io.UnsupportedEncodingException;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import ch.qos.logback.core.encoder.Encoder;

public class EventLogAppender extends AppenderBase<ILoggingEvent> {

	private String resource;
	private Encoder encoder;

	@Override
	@SuppressWarnings("unchecked")
	protected void append(final ILoggingEvent event) {
		if (resource == null || "".equals(resource)) {
			addError("Resource is not set for EventLogAppender.");
			return;
		}

		byte[] byteArrary = encoder.encode(event);
		String message = "";
		try {
			message = new String(byteArrary, "utf-8");
		} catch (UnsupportedEncodingException e) {
			addError(e.toString());
		}
		System.err.println("DEBUG: appending event message: " + message);

		String name = "EventLog";

		final String server = "."; // guess
		final String source = "example.log4jna_sample";
		final String application = "log4jna_sample";
		final String eventMessageFile = resource ; // "%SystemRoot%\\Microsoft.NET\\Framework\\v4.0.30319\\EventLogMessages.dll";
		final String categoryMessageFile = resource; // "%SystemRoot%\\Microsoft.NET\\Framework\\v4.0.30319\\EventLogMessages.dll";
		Win32EventLogAppender appender = Win32EventLogAppender.createAppender(name, server, source, application,
				eventMessageFile, categoryMessageFile);
		appender.append(message);

	}

	public String getResource() {
		return resource;
	}

	public void setResource(String value) {
		resource = value;
	}

	public void setEncoder(Encoder encoder) {
		this.encoder = encoder;
	}

}
