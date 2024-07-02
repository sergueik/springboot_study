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
public class FirstUniqueTest {
	private boolean debug = false;
	private String data = null;
	private int res = -1;

	@Test
	public void test1() throws Exception {
		debug = true;
		data = "leetcode";

		res = firstUniqChar(data);
		assertTrue(res == 0);
	}

	@Test
	public void test2() {
		debug = true;
		data = "loveleetcode";
		res = firstUniqChar(data);
		assertTrue(res == 2);
	}

	@Test
	public void test3() {
		debug = true;
		data = "loveleetcodcevtd";
		res = firstUniqChar(data);
		assertThat(res, is(-1));
	}

	private int firstUniqChar(String data) {
		if (data == null || data.isEmpty()) {
			return -1;
		}
		Map<Character, Integer> positions = new HashMap<>();
		char[] letters = data.toCharArray();
		for (int i = 0; i < letters.length; i++) {
			char ch = letters[i];
			if (!positions.containsKey(ch)) {
				// if (debug)
				//	System.err.println(String.format("candidate: %c %d", ch, i));
				positions.put(ch, i);
			} else
				positions.put(ch, -1);
		}
		int position = Integer.MAX_VALUE;

		for (char ch : positions.keySet()) {
			int i = positions.get(ch);
			if (i == -1)
				continue;
			if (debug)
				System.err.println(String.format("candidate: %c %d", ch, i));
			position = Math.min(position, i);

		}

		return position == Integer.MAX_VALUE ? -1 : position;
	}
}
