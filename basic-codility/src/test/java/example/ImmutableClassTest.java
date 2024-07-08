package example;
/**
 * Copyright 2024 Serguei Kouzmine
 */

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

		HashMap check = ce.getHashMap();
		String check1 = (String) (check.get(1));
		// Attempt change the local variable values

		HashMap<String, String> hmTest = ce.getHashMap();
		assertFalse(hmTest == check);
		hmTest.put("4", "new");
		assertTrue(hmTest.get(1) == check1);
		assertFalse(hmTest.containsKey(4));

	}
}
