package example;
/**
 * Copyright 2024 Serguei Kouzmine
 */

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertFalse;

import java.util.Set;
import java.util.HashSet;
import java.util.Map;
import java.util.HashMap;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;
// see also (cryptic):
// https://www.geeksforgeeks.org/length-of-the-longest-substring-without-repeating-characters/

public class LongestUniqueSubstringTest {

	private boolean debug = true;
	static String data = null;

	@Before
	public void before() {
		data = "ritmma12345alphabet";
	}

	@Test
	public void test1() {

		int result = lengthOfLongestContiguous(data);
		System.err.println("result: " + result);
		// assertTrue(data .indexOf(result) != -1);
		// assertEquals('8', result);
	}

	// @Ignore
	@Test
	public void test3() {
		int result = lengthOfLongestContiguous2(data);
		System.err.println("result: " + result);
		// assertTrue(data .indexOf(result) != -1);
		// assertEquals('8', result);
	}

	// @Ignore
	@Test
	public void test4() {
		int result = lengthOfLongestContiguous3(data);
		System.err.println("result: " + result);
		// assertTrue(data.indexOf(result) != -1);
		assertEquals(9, result);
	}

	// @Ignore
	@Test
	public void test5() {
		int result = lengthOfLongestContiguous3("abcderfhikk");
		System.err.println("result: " + result);
		// assertTrue(data.indexOf(result) != -1);
		assertEquals(10, result);
	}

	public int lengthOfLongestContiguous2(String data) {

		if (data == null || data.length() == 0)
			return 0;

		Set<Character> present = new HashSet<>();
		char[] letters = data.toCharArray();
		int cnt = 0;
		int max = 0;
		for (int s = 0; s < letters.length - 1; s++) {
			cnt = 0;
			present = new HashSet<>();
			for (int i = 0; i < letters.length - s; i++) {
				char ch = letters[i + s];

				if (!present.add(ch)) {
					max = (max > cnt) ? max : cnt;
					// https://stackoverflow.com/questions/16850937/converting-a-part-of-a-char-array-to-string-in-java
					if (debug) {
						System.err.println("char:  " + ch);
						System.err.println("candidate (2): " + new String(letters, i - cnt, cnt));
					}
					cnt = 1;
					present = new HashSet<>();
					present.add(ch);
					// break;
				} else {
					cnt++;
				}
			}
		}
		return max;
	}

	public int lengthOfLongestContiguous(String data) {
		if (data == null || data.length() == 0)
			return 0;
		char[] letters = data.toCharArray();
		Map<Character, Integer> positions = new HashMap<>();
		int previous = 0;
		int max = 0;

		for (int pos = 0; pos < data.length(); pos++) {
			char ch = letters[pos];

			previous = Math.max(previous, (positions.containsKey(ch)) ? positions.get(ch) + 1 : 0);

			max = Math.max(max, pos - previous + 1);
			if (positions.containsKey(ch))
				if (debug)
					System.err.println("candidate: " + new String(letters, previous, pos - previous + 1) + ", length: "
							+ (pos - previous + 1));
			positions.put(ch, pos);
		}
		if (debug)
			System.err.println("candidate: " + new String(letters, letters.length - max, max) + ", length: " + max);

		return max;
	}

	public int lengthOfLongestContiguous3(String data) {
		if (data == null || data.length() == 0)
			return 0;
		char[] letters = data.toCharArray();
		Map<Character, Integer> positions = new HashMap<>();
		int previous = 0;
		int maxlength = 0;
		int start = 0;
		int end = 0;
		for (int pos = 0; pos < data.length(); pos++) {
			char ch = letters[pos];
			if (positions.containsKey(ch)) {
				previous = Math.max(previous, positions.get(ch) + 1);
			}
			if (pos - previous + 1 > maxlength) {
				maxlength = pos - previous + 1;
				start = previous;
				if (debug)
					System.err.println(String.format("candidate: %s (length: %d)",
							new String(letters, previous, pos - previous + 1), (pos - previous + 1)));
			}
			positions.put(ch, pos);
		}
		if (debug)
			System.err.println("Result: " + new String(letters, start, maxlength) + ", length: " + maxlength);

		return maxlength;
	}

	public String findLongestContiguousAdvanced(String data) {
		return findLongestContiguousAdvanced(data, 1);

	}

	public String findLongestContiguousAdvanced(String s, int M) {
		if (s == null || s.length() <= M)
			return s;
		Map<Character, Integer> map = new HashMap<>();
		String res = "";
		int start = 0;
		int end = 0;
		while (start <= end && end < s.length()) {
			char c = s.charAt(end);
			// Update map
			map.put(c, map.containsKey(c) ? map.get(c) + 1 : 1);
			// Move start pointer
			while (map.size() > M) {
				char toRemove = s.charAt(start);
				if (map.containsKey(toRemove))
					map.put(toRemove, map.get(toRemove) - 1);
				if (!map.containsKey(toRemove) || map.get(toRemove) <= 0)
					map.remove(toRemove);
				start++;
			}
			// Update result
			String temp = s.substring(start, end + 1);
			if (res.length() < temp.length()) {
				res = temp;
			}
			end++;
		}
		return res;
	}

}
