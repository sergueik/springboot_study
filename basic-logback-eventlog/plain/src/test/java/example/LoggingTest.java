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

	
	private static final Logger log = LoggerFactory.getLogger(LoggingTest.class);
	private static int messageId = 42;
	private static final String EVENT_SOURCE = "example.log4jna_sample";
	private static final String LOGGER = "example.LoggingTest";

	/**
	 * Check whether EventViewer has the expected record for a specific period
	 * of time.
	 * 
	 * @param level
	 * @param eventLogType
	 * @param startedAt
	 * @param endedAt
	 */
	private void shouldBe(String logMessage, org.slf4j.event.Level level, EventLogType eventLogType, long startedAt, long endedAt) {
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

					String message = String.format("[%s]" + "\r\n" + "Logger: %s" + "\r\n" + "Message: %s",
							level.name(), LOGGER, logMessage);

					StringBuilder eventMessage = new StringBuilder();
					for (String str : record.getStrings()) {
						eventMessage.append(str);
					}

					assertEquals(message, eventMessage.toString());
				}
			}
			assertTrue(isfound, "Couldn't find record");
		} finally {
			iter.close();
		}
	}

	@Disabled
	@Test
	public void error() {
		long startedAt = System.currentTimeMillis() / 1000;
		log.error("test");
		long endedAt = System.currentTimeMillis() / 1000;
		shouldBe("test", Level.ERROR, EventLogType.Error, startedAt, endedAt);
	}
	@Disabled
	@Test
	public void warn() {
		long startedAt = System.currentTimeMillis() / 1000;
		log.warn("test");
		long endedAt = System.currentTimeMillis() / 1000;
		shouldBe("test", Level.WARN, EventLogType.Warning, startedAt, endedAt);
	}

	@Test
	public void info() {
		long startedAt = System.currentTimeMillis() / 1000;
		log.info("test");
		long endedAt = System.currentTimeMillis() / 1000;
		// 		shouldBe("test", Level.INFO, EventLogType.Informational, startedAt, endedAt);
	}

	@Disabled
	@Test
	public void debug() {
		long startedAt = System.currentTimeMillis() / 1000;
		log.debug("test");
		long endedAt = System.currentTimeMillis() / 1000;
		shouldBe("test", Level.DEBUG, EventLogType.Informational, startedAt, endedAt);
	}

	@Disabled
	@Test
	public void trace() {
		long startedAt = System.currentTimeMillis() / 1000;
		log.trace("test");
		long endedAt = System.currentTimeMillis() / 1000;
shouldBe("test", Level.TRACE, EventLogType.Informational, startedAt, endedAt);
	}
}
