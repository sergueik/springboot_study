package example;

/**
 * Copyright 2024 Serguei Kouzmine
 */
import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Disabled;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.junit.jupiter.api.Test;

import com.sun.jna.platform.win32.Advapi32Util.EventLogIterator;
import com.sun.jna.platform.win32.Advapi32Util.EventLogRecord;
import com.sun.jna.platform.win32.Advapi32Util.EventLogType;
import com.sun.jna.platform.win32.WinNT;
import org.slf4j.event.Level;

public class LoggingTest {

	private static final Logger log1 = LoggerFactory.getLogger(LoggingTest.class);
	// NOTE: for demonstration initialize custom logger
	static Logger log2 = (Logger) LoggerFactory.getLogger("eventlogAppender");

	private static int messageId = 42;
	private static final String EVENT_SOURCE = "example.log4jna_sample";
	private static final String LOGGER = "eventlogAppender";

	/**
	 * Check whether EventViewer has the expected record for a specific period
	 * of time.
	 * 
	 * @param level
	 * @param eventLogType
	 * @param startedAt
	 * @param endedAt
	 */
	private void shouldBe(String logMessage, Level level, EventLogType eventLogType, long startedAt, long endedAt) {
		shouldBe(logMessage, level, LOGGER, eventLogType, startedAt, endedAt);
	}

	private void shouldBe(String logMessage, Level level, String logger, EventLogType eventLogType, long startedAt,
			long endedAt) {
		EventLogIterator iter = new EventLogIterator(null, EVENT_SOURCE, WinNT.EVENTLOG_BACKWARDS_READ);
		try {
			assertTrue(iter.hasNext(), "There was no EventLog");

			boolean isfound = false;
			while (iter.hasNext() && !isfound) {
				EventLogRecord record = iter.next();

				if (record.getRecord().TimeWritten.longValue() >= startedAt
						&& record.getRecord().TimeWritten.longValue() <= endedAt) {
					isfound = true;
					if (record.getEventId() != messageId) {
						System.err.println("not the expected messageId: " + record.getEventId());
						continue;
					}
					assertEquals(EVENT_SOURCE, record.getSource());
					assertEquals(eventLogType, record.getType());

					String message = String.format("[%-5s] " + "\r\n" + " Logger: %s " + "\r\n" + " Message: %s",
							level.name(), logger, logMessage);

					StringBuilder eventMessage = new StringBuilder();
					for (String str : record.getStrings()) {
						eventMessage.append(str);
					}
					System.err.println(
							String.format("Expecting:\n" + "\"%s\"", message.replaceAll("\\r\\n", "\\\\r\\\\n")));
					System.err.println(String.format("Observed:\n" + "\"%s\"",
							eventMessage.toString().replaceAll("\\r\\n", "\\\\r\\\\n")));

					assertEquals(message, eventMessage.toString());
				}
			}
			assertTrue(isfound, "Couldn't find record");
		} finally {
			iter.close();
		}
	}

	private String message = null;

	@Test
	public void test1() {
		message = "test warn";
		long startedAt = System.currentTimeMillis() / 1000;
		log1.warn(message);
		long endedAt = System.currentTimeMillis() / 1000;
		shouldBe(message, Level.WARN, this.getClass().getCanonicalName(), EventLogType.Informational, startedAt,
				endedAt);
	}

	// TODO: info produces "WARN"
	@Disabled
	@Test
	public void test2() {
		message = "test info";
		long startedAt = System.currentTimeMillis() / 1000;
		log1.info(message);
		long endedAt = System.currentTimeMillis() / 1000;
		shouldBe(message, Level.WARN, this.getClass().getCanonicalName(), EventLogType.Informational, startedAt,
				endedAt);
	}

	@Test
	public void test7() {
		message = "test warn with selected logger";
		long startedAt = System.currentTimeMillis() / 1000;
		log2.warn(message);
		long endedAt = System.currentTimeMillis() / 1000;
		shouldBe(message, Level.WARN, EventLogType.Informational, startedAt, endedAt);
	}

	@Test
	public void test8() {
		message = "test info with selected logger";
		long startedAt = System.currentTimeMillis() / 1000;
		log2.info(message);
		long endedAt = System.currentTimeMillis() / 1000;
		shouldBe(message, Level.INFO, EventLogType.Informational, startedAt, endedAt);
	}

}
