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
	static String[] data = { "practice", "makes", "A", "coding", "perfect", "coding", "coding", "perfect", "coding",
			"it", "definitely", "makes" };
	static String word1;
	static String word2;

	@Before
	public void before() {
	}

	@Ignore
	@Test
	public void test1() {
		word1 = "makes";
		word2 = "coding";
		int result = shortestWordDistance(data, word1, word2);
		System.err.println("result: " + result);
		// assertTrue(data .indexOf(result) != -1);
		assertEquals(1, result);
	}

	@Test
	public void test2() {
		word1 = "makes";
		word2 = "coding";
		int result = shortestWordDistanceModified(data, word1, word2);
		System.err.println("result: " + result);
		assertEquals(2, result);
	}

	public int shortestWordDistanceModified(String[] words, String word1, String word2) {
		if (word1.isEmpty() || word2.isEmpty()) {
			return -1;
		}
		String word;
		int previous = -1;
		int shortest = words.length;
		for (int position = 0; position < words.length; position++) {
			word = words[position];
			if (word.equals(word1) || word.equals(word2)) {
				if (previous != -1) {
					if (words[previous].equals(word)) {
						if (debug)
							System.err.println("reset position to " + position);
						previous = position;
					} else {
						shortest = Math.min(position - previous, shortest);
						if (debug) {
							System.err.println(String.format("Consider %s %s", words[previous], word));
							System.err.println("compute distance to " + shortest);
						}
						// TODO
					}
				} else {
					previous = position;
				}

			}

		}
		return shortest;
	}

	public int shortestWordDistance(String[] words, String word1, String word2) {
		int prevIndex = -1;
		int min = words.length;
		for (int i = 0; i < words.length; i++) {
			if (words[i].equals(word1) || words[i].equals(word2)) {
				if (prevIndex != -1 && !words[prevIndex].equals(words[i])) {
					min = Math.min(i - prevIndex, min);
				}
				prevIndex = i;
			}
		}
		return min;
	}

}
