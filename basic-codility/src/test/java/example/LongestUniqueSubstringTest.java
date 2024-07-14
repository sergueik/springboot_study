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
		data = "ritmma12345alphabet";
		int result = lengthOfLongestContiguous(data);
		System.err.println("result: " + result);
		// assertTrue(data.indexOf(result) != -1);
		assertEquals(9, result);
	}

	// @Ignore
	@Test
	public void test5() {
		data = "abcderfhikk" ;
		int result = lengthOfLongestContiguous(data);
		System.err.println("result: " + result);
		// assertTrue(data.indexOf(result) != -1);
		assertEquals(10, result);
	}


	public int lengthOfLongestContiguous(String data) {
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
			int length = pos - previous + 1; 
			if (length > maxlength) {
				maxlength = length;
				start = previous;
				if (debug)
					System.err.println(String.format("candidate: %s (length: %d)",
							new String(letters, previous, length), length));
			}
			positions.put(ch, pos);
		}
		if (debug)
			System.err.println("Result: " + new String(letters, start, maxlength) + ", length: " + maxlength);

		return maxlength;
	}

}
