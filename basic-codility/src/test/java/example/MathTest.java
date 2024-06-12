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
import org.junit.Test;

public class MathTest {

	@Test
	public void test1() {
		String a = "10";
		String b = "10";

		String res = addBinary(a, b);

		assertThat(res, is("100"));
		// assertTrue(data.indexOf(String.valueOf(res)) == -1);
		// assertEquals('8', res);
	}

	@Test
	public void test2() {
		String a = "11";
		String b = "11";

		String res = addBinary(a, b);

		assertThat(res, is("110"));
		// assertTrue(data.indexOf(String.valueOf(res)) == -1);
		// assertEquals('8', res);
	}

	@Test
	public void test3() {
		String a = "111111";
		String b = "000001";

		String res = addBinary(a, b);

		assertThat(res, is("1000000"));
		// assertTrue(data.indexOf(String.valueOf(res)) == -1);
		// assertEquals('8', res);
	}

	public String addBinary(String a, String b) {
		char[] a1 = a.toCharArray();
		char[] b1 = b.toCharArray();
		StringBuilder result = new StringBuilder();
		char[] a2 = new char[a1.length];
		char[] b2 = new char[b1.length];
		int r1 = a1.length - 1;
		System.err.println("a: " + String.valueOf(a1));
		for (int i = 0; i != a1.length; i++) {
			a2[i] = a1[r1 - i];
		}
		System.err.println("a(*):" + String.valueOf(a2));
		r1 = b1.length - 1;
		System.err.println("b" + String.valueOf(b1));

		for (int i = 0; i != b1.length; i++) {
			b2[i] = b1[r1 - i];
		}
		System.err.println("b(*):" + String.valueOf(b2));
		int d = 0;
		// TODO: wrong loop = need to pad
		for (int i = 0, j = 0; i != a1.length && j != b1.length; i++, j++) {

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
