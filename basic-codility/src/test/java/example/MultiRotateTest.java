package example;
/**
 * Copyright 2024 Serguei Kouzmine
 */

import static org.hamcrest.CoreMatchers.is;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.hasKey;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.Stack;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

public class MultiRotateTest {

	@Test
	public void test1() {
		int shift = 3;
		String data = "abcdefgh";
		String result = rotate(data, shift);
		assertTrue(result.compareTo("defghabc") == 0);
	}

	@Test
	public void test2() {
		int shift = 3;
		String data = "abcdefgh";
		String result = rotate(data, shift);
		String expected = data.substring(shift).concat(data.substring(0, shift));
		System.err.println(String.format("%s vs. %s", result, expected));
		assertTrue(result.compareTo(expected) == 0);
		assertThat(result, is(expected));
	}

	@Test
	public void test3() {
		int shift = 10;
		String data = "abcdefgh";
		String result = rotate(data, shift);
		assertTrue(result.compareTo(data) == 0);
	}

	public String rotate(String data, int shift) {
		int size = data.length();
		if (shift >= size)
			return data;
		char[] letters = data.toCharArray();
		char[] rotated = new char[size];
		for (int pos = 0; pos != size; pos++) {
			int pos2 = pos + shift;
			pos2 %= size;
			// TODO: reverse math
			rotated[pos] = letters[pos2];
		}
		// return String.valueOf(rotated);
		return new String(rotated, 0, size);
	}

}
