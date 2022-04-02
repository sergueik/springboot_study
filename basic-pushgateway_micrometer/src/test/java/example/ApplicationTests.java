package example;

/**
 * Copyright 2022 Serguei Kouzmine
 */
import org.mockito.Mockito;
import io.micrometer.core.instrument.Counter;
import io.micrometer.core.instrument.MeterRegistry;

import static org.mockito.Mockito.when;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.doNothing;

import static org.hamcrest.Matchers.is;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.CoreMatchers.notNullValue;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import org.springframework.boot.test.context.SpringBootTest;

@SpringBootTest
class ApplicationTests {

	private static MeterRegistry mockRegistry;
	private ExampleService service;
	private static Counter mockCounter;
	private final static String body = "Hello, counter = 1.0";

	@BeforeAll
	public static void setupAll() {
		mockRegistry = Mockito.mock(MeterRegistry.class);
		mockCounter = Mockito.mock(Counter.class);
		// Mocking invocation of
		// MeterRegistry.counter(String name, String... tags): Counter
		// by examine the actual invocation by ExampleService
		when(mockRegistry.counter(any(String.class), any(String.class),
				any(String.class))).thenReturn(mockCounter);

		when(mockCounter.count()).thenReturn(1.0);
		// NOTE: mocking calls returning void is different
		// https://www.baeldung.com/mockito-void-methods

		// when(mockCounter.increment()).thenReturn();
		doNothing().when(mockCounter).increment();
	}

	@BeforeEach
	public void setupTest() {
		service = new ExampleService(mockRegistry);
	}

	private String response;

	@Test
	public void test1() {
		assertThat(service, notNullValue());
		try {
			response = service.getHello();
		} catch (Exception e) {
			// saved from invalid order of constructor and method mocks in the setup
			// to debug the failing injecton of the mock
			System.out.println("Exception: ");
			e.printStackTrace();
		}
		assertThat(response, notNullValue());
		assertThat(response, is(body));
	}
}
