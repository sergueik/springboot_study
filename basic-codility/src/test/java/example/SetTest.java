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
import org.junit.Test;

public class SetTest {

	@Test
	public void test3() {
		String data = "12356722240";

		char res = isAbsent(data);

		assertTrue(data.indexOf(String.valueOf(res)) == -1);
		assertEquals('8', res);
	}

	static String data1 = "12356722240";
	@Test
	public void test5() {
		String data2 = String.valueOf("12356722240");
		assertTrue(data1 == data2);
	}

	public char isAbsent(String data) {
		Set<Integer> present = new HashSet<>();
		char[] letters = data.toCharArray();
		for (int i = 0; i < data.length(); i++) {
			char ch = data.charAt(i);
			present.add(ch - '0');
			/*
			 * try { } catch (Exception e) { System.err.println("Exception : " +
			 * e.toString()); }
			 */
		}
		for (int i = 0; i < data.length(); i++) {
			if (!present.contains(i)) {
				System.err.println(String.format("Missing: %d", i));
				return (char) (i + '0');
			}

		}
		return '0';
	}

}
