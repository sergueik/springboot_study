package example;

import ch.qos.logback.classic.Logger;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.Appender;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.slf4j.LoggerFactory;

import static org.mockito.Mockito.*;
import org.mockito.ArgumentCaptor;

class FailingServiceTest {

	private static Logger logger = null;
	private static Appender<ILoggingEvent> appender = null;
	private static Service service = null;
	private ArgumentCaptor<ILoggingEvent> argumentCaptor = null;

	@SuppressWarnings("unchecked")
	@BeforeAll
	public static void beforeAll() {
		logger = (Logger) LoggerFactory.getLogger(Service.class);
		appender = Mockito.mock(Appender.class);
		logger.addAppender(appender);
		service = new Service();

	}

	@BeforeEach
	public void beforeEach() {
		appender.start();
	}

	@AfterEach
	public void afterEach() {
		appender.stop();
	}

	@Test
	void test1() {
		service.doWorkVerbosely();
		verify(appender).doAppend(any());
	}

	@Disabled
	@Test
	void test2() {
		service.doWorkQuietly();
		verify(appender, never()).doAppend(any());
	}
}
