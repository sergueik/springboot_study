package example.utils;

/**
 * Copyright 2026 Serguei Kouzmine
 */
import org.mockito.Mockito;
import org.mockito.invocation.InvocationOnMock;
import org.springframework.http.ResponseEntity;

import example.utils.ExampleService;

import static org.mockito.Mockito.when;

import java.io.File;
import java.util.Arrays;
import java.util.List;
import java.util.Random;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;

import static org.mockito.Mockito.any;

import static org.hamcrest.Matchers.is;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
//import org.junit.jupiter.api.Ignore;
import org.junit.jupiter.api.Test;

public class ServiceUnitTest {
	File file;
	ExampleService exampleService;
	int retries = 4;
	private AtomicInteger cnt = new AtomicInteger(0);
	private long[] sizes = { 10L, 20L, 30L, 40L };

	@BeforeEach
	public void setup() {
		file = Mockito.mock(File.class);
		exampleService = new ExampleService();
		// System.err.println(String.format("cnt: %d", cnt.intValue()));
	}

	@DisplayName("stable file")
	@Test
	public void test1() {
		when(file.length()).thenReturn(1L);
		when(file.lastModified()).thenReturn(1L);
		assertThat(exampleService.waitStable(file, retries), is(true));
	}

	@DisplayName("unstable file - size changes every time")
	@Test
	public void test2() {
		// NOTE: this will not work as intended:
		// when(file.length()).thenReturn(getFilesize());
		// The thenReturn() is evaluated once at stubbing time
		// The getFilesize() is not called every invocation
		when(file.length())
				.thenAnswer((InvocationOnMock invocation) -> sizes[cnt.getAndUpdate(n -> (n + 1) % sizes.length)]);
		// NOTE: file.lastModified() is constant during the test.
		// Using Math.random() (or any other function invocation) in thenReturn() would
		// be evaluated once at stubbing time,
		// creating a misleading illusion of variability. 
		// thenReturn() takes a value, not a supplier
		// Keep it deterministic.
		when(file.lastModified()).thenReturn(1L);
		assertThat(exampleService.waitStable(file, retries), is(false));
	}

	@DisplayName("stable file size stabilizes after the second call")
	@Test
	public void test3() {
		// Mockito returns next argument every call, then keep returning the last one
		when(file.length()).thenReturn(sizes[0], sizes[2]);
		when(file.lastModified()).thenReturn(1L);
		assertThat(exampleService.waitStable(file, retries), is(true));
	}

	private long getFilesize() {
		return (long) sizes[cnt.getAndUpdate(n -> (n + 1) % sizes.length)];
	}
}
