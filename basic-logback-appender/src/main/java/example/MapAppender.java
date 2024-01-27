package example;

import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.AppenderBase;

import java.io.UnsupportedEncodingException;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import ch.qos.logback.core.encoder.Encoder;

public class MapAppender extends AppenderBase<ILoggingEvent> {

	private final ConcurrentMap<String, ILoggingEvent> eventMap = new ConcurrentHashMap<>();

	private String prefix;
	private Encoder encoder;

	@Override
	@SuppressWarnings("unchecked")
	protected void append(final ILoggingEvent event) {
		if (prefix == null || "".equals(prefix)) {
			addError("Prefix is not set for MapAppender.");
			return;
		}
		
		byte[] byteArrary = encoder.encode(event);
		String message = "";
		try {
			message = new String(byteArrary, "utf-8");
			System.err.println(message);
		} catch (UnsupportedEncodingException e) {
			addError(e.toString());
		}

		eventMap.put(prefix + System.currentTimeMillis(), event);
	}

	public String getPrefix() {
		return prefix;
	}

	public void setPrefix(String value) {
		prefix = value;
	}

	public void setEncoder(Encoder encoder) {
		this.encoder = encoder;
	}

}
