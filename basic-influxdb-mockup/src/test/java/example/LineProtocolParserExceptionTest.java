package example;
/**
 * Copyright 2022 Serguei Kouzmine
 */

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import example.utils.LineProtocolParser;

public class LineProtocolParserExceptionTest {

	private static final LineProtocolParser utils = LineProtocolParser
			.getInstance();

	@BeforeEach
	public void beforeTest() {
		utils.setDebug(true);
	}

	// https://www.baeldung.com/junit-assert-exception#junit-5
	@Test
	void test1() throws Exception {
		Assertions.assertThrows(IllegalArgumentException.class, () -> {
			utils.extractPointFromLineProtocolLine(null, "");
		});
	}

	// checks the exception message,but not in clear way
	// when exception message is not matching expectation
	// this test will fail with
	// Unexpected exception type thrown
	// ==> expected: <java.lang.IllegalArgumentException> but was:
	// <org.opentest4j.AssertionFailedError>
	// changing Assertion.assertThrows argument to java.lang.Exception.class will
	// not resolve this behavior
	@Test
	void test2() throws Exception {
		Assertions.assertThrows(IllegalArgumentException.class, () -> {
			try {
				utils.extractPointFromLineProtocolLine(null, "");
			} catch (IllegalArgumentException e) {
				System.err.println("Exception message was: " + e.getMessage());
				Assertions.assertEquals("some message", e.getMessage());
				throw e;
			}
		});

	}

	// checks the exception message
	@Test
	void test3() {
		try {
			utils.extractPointFromLineProtocolLine(null, "");
		} catch (IllegalArgumentException e) {
			Assertions.assertEquals("some message", e.getMessage());
			throw e;
		}
	}

}
