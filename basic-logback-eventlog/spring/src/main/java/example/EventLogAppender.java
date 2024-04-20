package example;
/**
 * Copyright 2024 Serguei Kouzmine
 */
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.AppenderBase;

import java.io.UnsupportedEncodingException;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import ch.qos.logback.core.encoder.Encoder;

public class EventLogAppender extends AppenderBase<ILoggingEvent> {

	private String resource = "%SystemRoot%\\Microsoft.NET\\Framework\\v4.0.30319\\EventLogMessages.dll";
	private int id = 42;
	private String application = "log4jna_sample";
	private String source = "example.log4jna_sample";
	private Encoder encoder;

	@Override
	@SuppressWarnings("unchecked")
	protected void append(final ILoggingEvent event) {
		if (resource == null || "".equals(resource)) {
			addError("Resource is not set for EventLogAppender.");
			return;
		}
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

		final String server = ".";
		final String eventMessageFile = resource;
		final String categoryMessageFile = resource;
		/*
		System.err.println(String.format(
				"DEBUG: appending event:\n" + "\n" + "message: \"%s\"" + "\n" + "id: %d" + "\n" + "server: \"%s\""
						+ "\n" + "application: \"%s\"" + "\n" + "source: \"%s\"" + "\n" + "eventMessageFile: \"%s\""
						+ "\n" + "categoryMessageFile: \"%s\"",
				message, id, server, source, application, eventMessageFile, categoryMessageFile));
        */
		Win32EventLogAppender appender = Win32EventLogAppender.createAppender(id, server, source, application,
				eventMessageFile, categoryMessageFile);
		appender.append(message);

	}

	public String getResource() {
		return resource;
	}

	public void setResource(String value) {
		resource = value;
	}

	public String getSource() {
		return source;
	}

	public void setSource(String value) {
		source = value;
	}

	public int getId() {
		return id;
	}

	public void setId(int value) {
		id = value;
	}

	public String getApplication() {
		return application;
	}

	public void setApplication(String value) {
		application = value;
	}

	public void setEncoder(Encoder encoder) {
		this.encoder = encoder;
	}

}
