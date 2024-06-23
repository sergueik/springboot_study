package example;
/**
 * Copyright 2024 Serguei Kouzmine
 */

import static org.junit.Assert.*;

import java.io.IOException;
import java.util.Arrays;
import java.util.Stack;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

// https://www.geeksforgeeks.org/java-program-to-check-whether-two-strings-are-anagram-of-each-other/
public class AnagramTest {
	private boolean debug = false;

	@Test
	public void test1() throws Exception {
		debug = true;
		String data1 = "abda";
		String data2 = "bdaa";
		boolean res = areAnagram2(data1, data2);
		assertTrue(res);
	}
	@Test
	public void test2() throws Exception {
		debug = true;
		String data1 = "abda";
		String data2 = "bda";
		boolean res = areAnagram2(data1, data2);
		assertFalse(res);
	}

	@Test
	public void test3() throws Exception {
		debug = true;
		String data1 = "abda";
		String data2 = "bzda";
		boolean res = areAnagram2(data1, data2);
		assertFalse(res);
	}

	static boolean areAnagram2(String data1, String data2) {
		if (data1.length() != data2.length())
			return false;

		char[] letters1 = data1.toCharArray();
		char[] letters2 = data2.toCharArray();
		Arrays.sort(letters1);
		Arrays.sort(letters2);

		if ((new String(letters1)).compareTo(new String(letters2)) == 0)
			return true;
		else
			return false;
	}

	static boolean areAnagram1(String data1, String data2) {
		if (data1.length() != data2.length())
			return false;
		char[] letters1 = data1.toCharArray();
		char[] letters2 = data2.toCharArray();
		Arrays.sort(letters1);
		Arrays.sort(letters2);

		for (int i = 0; i < data1.length(); i++)
			if (letters1[i] != letters1[i])
				return false;

		return true;
	}

}
