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
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

public class LongestUniqueSubstringTest {

	private boolean debug = false;

	@Test
	public void test1() {
		String data = "alphabetritmma123456789zyxwa";

		int result = lengthOfLongestContiguous(data);
		System.err.println("result: " + result);
		// assertTrue(data .indexOf(result) != -1);
		// assertEquals('8', result);
	}

	// @Ignore
	@Test
	public void test3() {
		String data = "alphabetritmma123456789zyxwa";

		int result = lengthOfLongestContiguous2(data);
		System.err.println("result: " + result);
		// assertTrue(data .indexOf(result) != -1);
		// assertEquals('8', result);
	}

	public int lengthOfLongestContiguous2(String data) {

		if (data == null || data.length() == 0)
			return 0;

		Set<Character> present = new HashSet<>();
		char[] letters = data.toCharArray();
		StringBuffer stringBuffer = new StringBuffer();
		int cnt = 0;
		int max = 0;
		for (int s = 0; s < letters.length - 1; s++) {
			cnt = 0;
			present = new HashSet<>();
			for (int i = 0; i < letters.length - s; i++) {
				char ch = letters[i + s];
				stringBuffer.append(ch);

				if (!present.add(ch)) {
					String result = stringBuffer.toString();
					result = result.substring(0, result.length() - 1);
					System.err.println(String.format("candidate: %s", result));
					max = (max > cnt) ? max : cnt;
					stringBuffer = new StringBuffer();
					// https://stackoverflow.com/questions/16850937/converting-a-part-of-a-char-array-to-string-in-java
					// if (debug)
					System.err.println(new String(letters, s, i ));
					cnt = 1;
					present = new HashSet<>();
					present.add(ch);
					stringBuffer.append(ch);
				} else {
					cnt++;
				}
			}
		}
		return max;
	}

	public static int lengthOfLongestContiguous(String data) {
		if (data == null || data.length() == 0)
			return 0;
		char[] letters = data.toCharArray();
		Map<Character, Integer> positions = new HashMap<>();
		int start = 0;
		int max = 0;

		for (int i = 0; i < data.length(); i++) {
			char c = data.charAt(i);

			start = Math.max(start, (positions.containsKey(c)) ? positions.get(c) + 1 : 0);

			max = Math.max(max, i - start + 1);
			System.err.println(String.format("%d %d %d", start, i, letters.length));
			System.err.println(new String(letters, start, i - start));
			positions.put(c, i);
		}
		return max;
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
