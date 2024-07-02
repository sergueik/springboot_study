package example;
/**
 * Copyright 2024 Serguei Kouzmine
 */

import static org.junit.Assert.*;

import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Stack;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.hasKey;

// https://www.geeksforgeeks.org/java-program-to-check-whether-two-strings-are-anagram-of-each-other/
public class ReverseNumberTest {
	private boolean debug = false;
	private int data = 0;
	private int res = -1;
	private boolean result = false;

	@Test
	public void test1() throws Exception {
		debug = true;
		data = 12345;

		res = reverse(data);
		assertTrue(res == 54321);
	}

	@Test
	public void test2() throws Exception {
		debug = true;
		data = 123454321;

		result = isPalindromeNumber2(data);
		assertTrue(result);
	}

	@Test
	public void test3() throws Exception {
		debug = true;
		data = 12344321;

		result = isPalindromeNumber2(data);
		assertTrue(result);
	}

	@Test
	public void test4() throws Exception {
		debug = true;
		data = 123454321;

		res = reverse(data);
		assertTrue(res == 123454321);
	}

	public boolean isPalindromeNumber2(int num) {
		int div = 1;
		if (num < 10)
			return true;
		while (num / div >= 10)
			div *= 10;
		if (debug)
			System.err.println("div= " + div);
		while (num > 10 && div != 0) {
			int high_digit = num / div;
			int low_digit = num % 10;
			if (high_digit != low_digit)
				return false;
			num = (num % div) / 10;
			if (debug)
				System.err.println("num = " + num);
			div /= 100;
			if (debug)
				System.err.println("div = " + div);
		}
		return true;
	}

	private int reverse(int data) {
		long rev = 0;
		while (data != 0) {
			rev = rev * 10 + data % 10;
			data /= 10;
		}
		return (int) rev;
	}
}

