package example;

import ch.qos.logback.classic.Logger;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.Appender;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.slf4j.LoggerFactory;

import static org.mockito.Mockito.*;
import org.mockito.ArgumentCaptor;

class ServiceTest {
	// NOTE: Avoid static test fixtures for mocks or appenders.
	// Static fields are shared across all test methods, so interactions recorded in one test can leak into another. 
	// Frameworks like Mockito keep invocation history on the mock, which means later tests may observe leftover calls 
	// unless the mock is explicitly reset. This breaks test isolation and can produce order-dependent failures. 
	// Prefer instance fields initialized in @BeforeEach so every test runs with a clean state.
	private Logger logger = null;
	private Appender<ILoggingEvent> appender = null;
	private Service service = null;
	private ArgumentCaptor<ILoggingEvent> argumentCaptor = null;

	@BeforeEach
	@SuppressWarnings("unchecked")
	public void beforeEach() {
		logger = (Logger) LoggerFactory.getLogger(Service.class);
		appender = Mockito.mock(Appender.class);
		logger.addAppender(appender);
		service = new Service();
		appender.start();
		argumentCaptor = ArgumentCaptor.forClass(ILoggingEvent.class);
	}

	@AfterEach
	public void afterEach() {
		appender.stop();
	}

	@Test
	void test1() {
		service.doWorkVerbosely();
		verify(appender).doAppend(any());
		verify(appender).doAppend(argumentCaptor.capture());

		ILoggingEvent loggingEvent = argumentCaptor.getValue();

		assertThat(loggingEvent.getFormattedMessage(), is("starting work"));
		// assertThat(loggingEvent.hasCallerData(), is(true));
		/*
		 * StackTraceElement[] callerData = event.getCallerData(); if (callerData !=
		 * null && callerData.length > 0) { StackTraceElement caller = callerData[0];
		 * String className = caller.getClassName(); // Get the caller class name String
		 * methodName = caller.getMethodName(); // Get the caller method name int
		 * lineNumber = caller.getLineNumber(); // Get the line number
		 * 
		 * System.out.println("Caller Class: " + className);
		 * System.out.println("Caller Method: " + methodName);
		 * System.out.println("Line Number: " + lineNumber); }
		 */
		// assertThat(loggingEvent.getCallerData(), is("zzzz"));
		// assertThat(loggingEvent.getCallerData()[0].getMethodName(), is("zzzz"));
		// ArrayIndexOutOfBounds Index 0 out of bounds for length 0
	}

	@Test
	void test2() {
		service.doWorkQuietly();
		verify(appender, never()).doAppend(any());
	}
}
