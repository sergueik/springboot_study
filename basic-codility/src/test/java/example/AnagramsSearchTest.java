package example;
/**
 * Copyright 2024 Serguei Kouzmine
 */

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

// better matchers
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.hasKey;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.hasKey;

import java.util.Set;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

// Anagram search example
public class AnagramsSearchTest {

	private boolean debug = true;

	@Test
	public void test1() throws Exception {
		String[] strs = { "dog", "dot", "cog", "log", "god", "tod" };
		System.err.println(Arrays.asList(strs));
		List<String> res = anagrams(strs);
		System.err.println(res.toString());
	}

	@Test
	public void test2() throws Exception {
		String[] strs = { "dog", "dot", "cog", "log", "god", "tod" };
		System.err.println(Arrays.asList(strs));
		List<String> res = anagrams2(strs);
		System.err.println(res.toString());
	}

	// based on:
	// NOTE: cryptic
	// TODO: add debugging logs to explain the process
	public List<String> anagrams(String[] strs) {
		List<String> res = new ArrayList<>();
		if (strs == null || strs.length == 0)
			return res;
		Map<String, Integer> map = new HashMap<>();
		for (int i = 0; i < strs.length; i++) { // traverse the array
			/* generate key */
			char[] word = strs[i].toCharArray();
			Arrays.sort(word);
			String key = new String(word);
			if (map.containsKey(key)) {
				res.add(strs[i]); // add this string
				if (map.get(key) >= 0) { // key string not added
					res.add(strs[map.get(key)]);
					map.put(key, -1); // mark already added as -1
				}
			} else
				map.put(key, i); // first put sorted string and index
		}
		if (debug)
			for (String word : map.keySet()) {
				System.err.println(String.format("%s %s", word, Integer.toString(map.get(word))));
			}
		return res;
	}

	public List<String> anagrams2(String[] words) {
		List<String> result = new ArrayList<>();
		if (words == null || words.length == 0)
			return result;
		Map<String, List<String>> map = new HashMap<>();
		for (int i = 0; i < words.length; i++) {
			String word = words[i];

			// key is the sorted string
			char[] chars = word.toCharArray();
			Arrays.sort(chars);
			String key = new String(chars);

			if (!map.containsKey(key))
				map.put(key, new ArrayList<>());
			List<String> row = map.get(key);
			row.add(word); // add this string
			map.put(key, row);
		}
		if (debug)
			for (String key : map.keySet()) {
				List<String> row = map.get(key);
				if (debug)
					System.err.println(String.format("%s %s", key, Arrays.asList(row)));
				if (row != null && row.size() > 1)
					result.addAll(row);

			}
		return result;
	}

}
