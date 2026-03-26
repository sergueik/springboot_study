package example.utils;

import java.util.Random;

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
	private Long[] data = { 10L, 20L, 30L, 40L };

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
				.thenAnswer((InvocationOnMock invocation) -> data[cnt.getAndUpdate(n -> (n + 1) % data.length)]);
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
		when(file.length()).thenReturn(data[0], data[2]);
		when(file.lastModified()).thenReturn(1L);
		assertThat(exampleService.waitStable(file, retries), is(true));
	}

	@DisplayName("stable file size failed after exhausting probe calls")
	@Test
	public void test4() {
		when(file.length()).thenReturn(data[0], data[1], data[2], data[3], data[0]);
		when(file.lastModified()).thenReturn(1L);
		assertThat(exampleService.waitStable(file, retries), is(false));
	}

	// Java does not have a "spread" because is a statically typed language
	@DisplayName("stable file size failed  with  sampling with Repetition")
	@Test
	public void test5() {
		// send one scalar + one array so it can bind to thenReturn(T, T...)
		when(file.length()).thenReturn(data[0], (Long[]) makeSample(data, retries, false));
		when(file.lastModified()).thenReturn(1L);
		assertThat(exampleService.waitStable(file, retries), is(false));
	}

	// NOTE: this is how to NOT do it - it wil not work
	private long getFilesize() {
		return (long) data[cnt.getAndUpdate(n -> (n + 1) % data.length)];
	}

	// Sample with replacements
	public static <T> T[] makeSample(T[] data, int count, boolean makerandom) {
		if (data == null || data.length == 0) {
			return null; // Or throw an exception as appropriate
		}

		T[] result = Arrays.copyOf(data, count);
		Random random = new Random();
		int sourceLength = data.length;

		for (int cnt = 0; cnt < count; cnt++) {
			// optionally generate a random index within the bounds of the source array
			int index = makerandom ? random.nextInt(sourceLength - 1) : cnt % sourceLength;
			result[cnt] = data[index];
		}
		return result;
	}

}
