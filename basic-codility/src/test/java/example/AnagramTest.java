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

	@Test
	public void test4() throws Exception {
		debug = true;
		String data1 = "abzda";
		String data2 = "bdaza";
		boolean res = areAnagram3(data1, data2);
		assertTrue(res);
	}

	@Test
	public void test5() throws Exception {
		debug = true;
		String data1 = "abdaz";
		String data2 = "bzda";
		boolean res = areAnagram3(data1, data2);
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

	static boolean areAnagram3(String data1, String data2) {
		if (data1.length() != data2.length())
			return false;

		char[] letters1 = data1.toCharArray();
		char[] letters2 = data2.toCharArray();

		Map<Character, Integer> positions1 = new HashMap<>();
		for (int i = 0; i < letters1.length; i++) {
			char ch = letters1[i];
			if (!positions1.containsKey(ch)) {
				positions1.put(ch, 1);
			} else
				positions1.put(ch, positions1.get(ch) + 1);
		}

		Map<Character, Integer> positions2 = new HashMap<>();
		for (int i = 0; i < letters2.length; i++) {
			char ch = letters2[i];
			if (!positions2.containsKey(ch)) {
				positions2.put(ch, 1);
			} else
				positions2.put(ch, positions2.get(ch) + 1);
		}
		for (char ch : positions1.keySet()) {
			if (!positions2.containsKey(ch) || positions2.get(ch) != positions1.get(ch)) {
				return false;
			}
		}
		for (char ch : positions2.keySet()) {
			if (!positions1.containsKey(ch) || positions1.get(ch) != positions2.get(ch)) {
				return false;
			}
		}
		return true;

	}

}