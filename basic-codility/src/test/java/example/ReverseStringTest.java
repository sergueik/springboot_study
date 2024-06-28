package example;
/**
 * Copyright 2024 Serguei Kouzmine
 */

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertFalse;

import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.ConcurrentModificationException;
import java.util.HashMap;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.hasKey;

// see also: https://www.baeldung.com/java-concurrentmodificationexception
public class ReverseStringTest {

	private boolean debug = true;

	@Before
	public void before() {
	}

	@Test
	public void test1() {

		final String data = "abcdefghijklmn";
		assertThat(reverse(data), is(new StringBuilder(data).reverse().toString()));
	}

	@Test
	public void test2() {

		final String data = "abcdefghijklmno";
		assertThat(reverse(data), is(new StringBuilder(data).reverse().toString()));
	}

	@Test
	public void test3() {

		final String data = "abcdefghijklmno";
		assertThat(reverseBytes(data), is(new StringBuilder(data).reverse().toString()));
	}

	@Test
	public void test4() {

		final String data = "abcdefghijklmno";
		assertThat(reverseBytes(data), is(new StringBuilder(data).reverse().toString()));
	}

	public String reverse(String data) {
		if (data == null || data.length() < 2)
			return data;
		char[] letters = data.toCharArray();
		char letter;
		int data_length = data.length();
		for (int i = 0; i != data_length / 2; i++) {
			letter = letters[i];
			letters[i] = letters[data_length - i - 1];
			letters[data_length - i - 1] = letter;
		}
		return new String(letters, 0, data_length);
	}

	public String reverseBytes(String data) {
		if (data == null || data.length() < 2)
			return data;
		byte[] bytes = data.getBytes();
		int data_length = data.length();
		for (int i = 0; i != data_length / 2; i++) {
			int j = data_length - i - 1;
			bytes[i] ^= bytes[j];
			bytes[j] ^= bytes[i];
			bytes[i] ^= bytes[j];
		}
		return new String(bytes, 0, data_length);
	}
}
