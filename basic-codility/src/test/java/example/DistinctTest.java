package example;
/**
 * Copyright 2024 Serguei Kouzmine
 */

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.Set;
import java.util.HashSet;
import java.util.Arrays;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

public class DistinctTest {

	private boolean debug = false;

	// @Ignore
	@Test
	public void test1() {
		String data = "128356790";

		int res = CountDistinct1(data);

		assertTrue(res == 9);
	}

	// @Ignore
	@Test
	public void test2() {
		String data = "012233555564";
		int res = CountDistinct1(data);
		assertTrue(res == 7);
	}

	// @Ignore
	@Test
	public void test3() {
		String data = "";
		int res = CountDistinct1(data);
		assertTrue(res == 0);
	}

	// @Ignore
	@Test
	public void test54() {
		String data = "012233555564";
		int res = CountDistinct2(data);
		assertTrue(res == 7);
	}

	public int CountDistinct1(String data) {
		Set<Character> present = new HashSet<>();
		char[] letters = data.toCharArray();
		for (int i = 0; i < letters.length; i++) {
			char ch = letters[i];
			if (debug)
				System.err.println(String.format("Checking: %c", ch));
			present.add(ch);

		}
		return present.size();
	}

	public int CountDistinct2(String data) {
		char[] letters = data.toCharArray();

		if (letters.length == 0)
			return 0;

		int result = 1;

		Arrays.sort(letters);

		for (int pos = 0; pos < letters.length - 1; pos++) {
			if (letters[pos] != letters[pos + 1])
				result++;
		}
		// comment the flow
		return result;
	}

}
