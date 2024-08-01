package example;
/**
 * Copyright 2024 Serguei Kouzmine
 */

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.any;
import static org.hamcrest.Matchers.hasEntry;
import static org.hamcrest.Matchers.hasKey;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.HashMap;

import org.junit.Test;

// https://www.digitalocean.com/community/tutorials/how-to-create-immutable-class-in-java
public class ImmutableClassTest {

	private boolean debug = true;

	//
	@Test
	public void test1() {
		HashMap<String, String> hashMap = new HashMap<>();
		hashMap.put("1", "first");
		hashMap.put("2", "second");

		String name = "original";
		int id = 10;

		ExampleClass ce = new ExampleClass(id, name, hashMap);

		HashMap<String, String> hashMap1 = ce.getHashMap();
		String check1 = hashMap1.get(1);
		// Attempt change the local variable values

		HashMap<String, String> hashMap2 = ce.getHashMap();
		assertFalse(hashMap2 == hashMap1);
		assertTrue(hashMap2.get(1) == check1);
		hashMap2.put("4", "new");
		assertFalse(hashMap1.containsKey(4));

	}

	@Test
	public void test2() {
		HashMap<String, String> hashMap = new HashMap<>();
		hashMap.put("1", "first");
		hashMap.put("2", "second");

		String name = "original";
		int id = 10;

		ExampleClass ce = new ExampleClass(id, name, hashMap);

		HashMap<String, String> hashMap1 = ce.getHashMap();
		String check1 = hashMap1.get(1);
		// Attempt change the local variable values

		HashMap<String, String> hashMap2 = ce.getHashMap();
		assertThat(hashMap2 == hashMap1, is(false));
		assertThat(hashMap2, is(hashMap1));
		assertThat(hashMap2.get(1), is(check1));
		hashMap2.put("4", "new");
		assertThat(hashMap1, not(hasKey("4")));
		assertThat(hashMap1, not(hasEntry(is("4"), any(String.class))));

	}
}
