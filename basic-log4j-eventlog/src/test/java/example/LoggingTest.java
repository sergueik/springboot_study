package example;

import static org.junit.jupiter.api.Assertions.*;

import org.apache.logging.log4j.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.jupiter.api.Test;

import com.sun.jna.platform.win32.Advapi32Util.EventLogIterator;
import com.sun.jna.platform.win32.Advapi32Util.EventLogRecord;
import com.sun.jna.platform.win32.Advapi32Util.EventLogType;
import com.sun.jna.platform.win32.WinNT;

public class LoggingTest {

	private static final Logger log = LogManager.getLogger(LoggingTest.class);

	private static final String EVENT_SOURCE = "example.log4jna_sample";
	private static final String THREAD = "main";
	private static final String LOGGER = "example.LoggingTest";

	/**
	 * Check whether EventViewer has the expected record for a specific period of
	 * time.
	 * 
	 * @param level
	 * @param eventLogType
	 * @param startedAt
	 * @param endedAt
	 */
	private void shouldBe(Level level, EventLogType eventLogType, long startedAt,
			long endedAt) {
		EventLogIterator iter = new EventLogIterator(null, EVENT_SOURCE,
				WinNT.EVENTLOG_BACKWARDS_READ);
		try {
			assertTrue(iter.hasNext(), "There was no EventLog");

			boolean isfound = false;
			while (iter.hasNext() && !isfound) {
				EventLogRecord record = iter.next();

				if (record.getRecord().TimeWritten.longValue() >= startedAt
						&& record.getRecord().TimeWritten.longValue() <= endedAt) {
					isfound = true;

					assertEquals(EVENT_SOURCE, record.getSource());
					assertEquals(eventLogType, record.getType());

					String message = String.format(
							"Thread: %s\r\nLogger: %s\r\nMessage: %s\r\n", THREAD, LOGGER,
							level.name().toLowerCase());

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

	@Test
	public void fatal() {
		long startedAt = System.currentTimeMillis() / 1000;
		log.fatal("fatal");
		long endedAt = System.currentTimeMillis() / 1000;
		shouldBe(Level.FATAL, EventLogType.Error, startedAt, endedAt);
	}

	@Test
	public void error() {
		long startedAt = System.currentTimeMillis() / 1000;
		log.error("error");
		long endedAt = System.currentTimeMillis() / 1000;
		shouldBe(Level.ERROR, EventLogType.Error, startedAt, endedAt);
	}

	@Test
	public void warn() {
		long startedAt = System.currentTimeMillis() / 1000;
		log.warn("warn");
		long endedAt = System.currentTimeMillis() / 1000;
		shouldBe(Level.WARN, EventLogType.Warning, startedAt, endedAt);
	}

	@Test
	public void info() {
		long startedAt = System.currentTimeMillis() / 1000;
		log.info("info");
		long endedAt = System.currentTimeMillis() / 1000;
		shouldBe(Level.INFO, EventLogType.Informational, startedAt, endedAt);
	}

	@Test
	public void debug() {
		long startedAt = System.currentTimeMillis() / 1000;
		log.debug("debug");
		long endedAt = System.currentTimeMillis() / 1000;
		shouldBe(Level.DEBUG, EventLogType.Informational, startedAt, endedAt);
	}

	@Test
	public void trace() {
		long startedAt = System.currentTimeMillis() / 1000;
		log.trace("trace");
		long endedAt = System.currentTimeMillis() / 1000;
		shouldBe(Level.TRACE, EventLogType.Informational, startedAt, endedAt);
	}
}
