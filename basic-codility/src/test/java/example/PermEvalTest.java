package example;
/**
 * Copyright 2024 Serguei Kouzmine
 */

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.Set;
import java.util.HashSet;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

public class PermEvalTest {

	private boolean debug = false;

	// @Ignore
	@Test
	public void test3() {
		String data = "128356790";

		int res = isPerm(data);

		assertTrue(res == 0);
	}

	// @Ignore
	@Test
	public void test1() {
		String data = "0123564";
		int res = isPerm(data);
		assertTrue(res == 1);
	}

	// @Ignore
	@Test
	public void test2() {
		String data = "12356722240";
		int res = isPerm(data);
		assertTrue(res == 0);
	}

	public int isPerm(String data) {
		Set<Integer> present = new HashSet<>();
		char[] letters = data.toCharArray();
		for (int i = 0; i < letters.length; i++) {
			char ch = letters[i];
			if (debug)
				System.err.println(String.format("Checking: %c", ch));
			if (present.add(ch - '0') == false) {
				if (debug)
					System.err.println(String.format("Repetition: %c", i, ch));
				return 0;
			}
		}
		for (int i = 0; i < data.length(); i++) {
			if (!present.contains(i)) {
				if (debug)
					System.err.println(String.format("Missing: %d", i));
				return 0;

			}

		}
		return 1;
	}

}
