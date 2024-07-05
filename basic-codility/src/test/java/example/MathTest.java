package example;
/**
 * Copyright 2024 Serguei Kouzmine
 */

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.hasKey;

import java.util.Set;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

public class MathTest {

	private final static int radix = 2;
	private String a = null;
	private String b = null;
	private String res = null;

	@Test
	public void test2() {
		a = Integer.toString(2, radix);
		b = Integer.toString(2, radix);

		res = addBinary(a, b);

		assertThat(Integer.parseInt(res, radix), is(4));
	}

	@Test
	public void test3() {
		String a = "11";
		String b = "11";
		res = addBinary(a, b);
		assertThat(res, is("110"));
	}

	@Test
	public void test4() {
		a = Integer.toString(3, radix);
		b = Integer.toString(3, radix);

		res = addBinary(a, b);
		assertThat(Integer.parseInt(res, radix), is(6));
	}

	@Test
	public void test5() {
		a = "111111";
		b = "000001";
		System.err.println("test5 a: " + a);
		System.err.println("test5 b: " + b);

		res = addBinary(a, b);
		System.err.println("test5 res: " + res);

		assertThat(res, is("1000000"));
	}

	@Test
	public void test6() {
		a = Integer.toString(63, radix);
		b = Integer.toString(1, radix);
		System.err.println("test6 a: " + a);
		System.err.println("test6 b: " + b);
		String res = addBinary(a, b);
		System.err.println("test6 res: " + res);

		assertThat(Integer.parseInt(res, radix), is(64));
	}

	// @Ignore
	// overflow ?
	@Test
	public void test7() {
		a = Integer.toString(127, radix);
		b = Integer.toString(1, radix);
		System.err.println("test7 a: " + a);
		System.err.println("test7 b: " + b);
		String res = addBinary(a, b);
		System.err.println("test7 res: " + res);
		assertThat(Integer.parseInt(res, radix), is(128));
	}

	public String addBinary(String a, String b) {
		char[] a1 = a.toCharArray();
		char[] b1 = b.toCharArray();
		StringBuilder result = new StringBuilder();
		// pad
		int size = Math.max(a1.length, b1.length);

		char[] a2 = new char[size];
		char[] b2 = new char[size];
		System.err.println("a: " + String.valueOf(a1));

		for (int i = 0; i != size; i++) {
			a2[i] = '0';
		}
		System.err.println("a2: " + String.valueOf(a2));
		for (int i = 0; i != size; i++) {
			b2[i] = '0';
		}
		System.err.println("b2: " + String.valueOf(b2));
		// reverse
		int r1 = a1.length - 1;
		for (int i = 0; i != a1.length; i++) {
			a2[i] = a1[r1 - i];
		}
		System.err.println("a(*):" + String.valueOf(a2));
		r1 = b1.length - 1;

		System.err.println("b: " + String.valueOf(b1));

		for (int i = 0; i != b1.length; i++) {
			b2[i] = b1[r1 - i];
		}
		System.err.println("b(*): " + String.valueOf(b2));

		int d = 0;
		// TODO: wrong loop = need to pad
		for (int i = 0, j = 0; i != size && j != size; i++, j++) {

			char c = '0';
			int a3 = a2[i] - '0';
			int b3 = b2[j] - '0';
			System.err.println(String.format("Adding: %d %d %d", a3, b3, d));
			if ((a3 + b3 + d) == 3) {
				c = '1';
				d = 1;
			}
			if ((a3 + b3 + d) == 2) {
				c = '0';
				d = 1;
			}
			if ((a3 + b3 + d) == 1) {
				c = '1';
				d = 0;
			}
			if ((a3 + b3 + d) == 0) {
				c = '0';
				d = 0;
			}

			System.err.println(String.format("=> %c (%d)", c, d));
			result.append(c);
		}
		if (d == 1) {
			result.append('1');
		}
		return result.reverse().toString();
	}

}
