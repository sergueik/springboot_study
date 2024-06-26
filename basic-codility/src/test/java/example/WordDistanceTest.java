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

public class WordDistanceTest {

	private boolean debug = true;
	// NOTE: Array constants can only be used in initializers
	static String[] data = { "practice", "makes", "A", "coding", "very", "perfect", "coding", "coding", "perfect",
			"coding", "it", "definitely", "makes" };
	static String word1;
	static String word2;

	@Before
	public void before() {
	}

	// @Ignore
	@Test
	public void test1() {
		word1 = "coding";
		word2 = "makes";
		int result = shortestWordDistance(data, word1, word2);
		System.err.println("result: " + result);
		// assertTrue(data .indexOf(result) != -1);
		assertEquals(2, result);
	}

	@Test
	public void test2() {
		word1 = "makes";
		word2 = "coding";
		int result = shortestWordDistance(data, word1, word2);
		System.err.println("result: " + result);
		assertEquals(2, result);
	}

	public int shortestWordDistance(String[] words, String word1, String word2) {
		if (word1.isEmpty() || word2.isEmpty()) {
			return -1;
		}
		String word;
		int position1 = -1;
		int position2 = -1;
		int shortest = words.length;
		for (int position = 0; position < words.length; position++) {
			word = words[position];
			if (word.equals(word1)) {
				position1 = position;
				if (debug)
					System.err.println("reset word1 position to " + position1);
			}
			if (word.equals(word2)) {
				position2 = position;
				if (debug)
					System.err.println("reset word2 position to " + position2);
			}
			if (position1 != -1 && position2 != -1) {
				shortest = Math.min(Math.abs(position1 - position2), shortest);
				if (debug) {
					System.err.println("compute distance to " + shortest);
				}

			}

		}
		return shortest;

	}

}
