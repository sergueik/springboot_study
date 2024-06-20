package example;
/**
 * Copyright 2024 Serguei Kouzmine
 */

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertFalse;

import java.util.Set;
import java.util.HashSet;
import java.util.Arrays;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

public class SetTest {
	private boolean debug = true;

	@Test
	public void test3() {
		String data = "12356722240";

		char res = isAbsent(data);

		assertTrue(data.indexOf(String.valueOf(res)) == -1);
		assertEquals('8', res);
	}

	static String data2 = String.valueOf("3142354231");

	@Test
	public void test6() {
		String data = data2;
		int result = isFull(6, data);
		assertTrue(result == 6);
		data = data2.substring(0, 3) + "11111" + "222" + data2.substring(3);
		result = isFull(6, data);
		assertTrue(result == 14);
		data = data2 + "11111";
		result = isFull(6, data);
		assertTrue(result == 6);
	}

	static String data1 = "12356722240";

	// https://www.shiksha.com/online-courses/articles/difference-between-equals-function-and-equal-operator-in-java-blogId-156757#:~:text=equals()%20and%20==%20in%20Java?,customized%20in%20user-defined%20classes.
	@Test
	public void test5() {
		String data2 = String.valueOf("12356722240");
		assertTrue(data1 == data2);
		String data3 = "1235672224";
		data3 = data3 + "0";
		assertFalse(data1 == data3);
		assertTrue(data1 == data2);
		String data4 = new String("12356722240");
		assertFalse(data1 == data4);

	}

	public int isFull(int X, String data) {
		Set<Integer> present = new HashSet<>();
		char[] letters = data.toCharArray();
		// Leaves fall from a tree onto the surface of the river.
		for (int i = 0; i < data.length(); i++) {
			char ch = letters[i];
			present.add(ch - '0');

			if (present.size() == X - 1) {
				// when all the positions from 1 to X are covered by leaves
				System.err.println("Done on: " + i);
				return i + 1;
			}
		}
		return -1;
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

	// @Ignore
	@Test
	public void test4() {

		String data = "128356790";

		int res = isPerm2(data);

		assertTrue(res == 0);
	}

	// @Ignore
	@Test
	public void test7() {
		String data = "0123564";
		int res = isPerm2(data);
		assertTrue(res == 1);
	}

	// @Ignore
	@Test
	public void test8() {
		String data = "12356722240";
		int res = isPerm2(data);
		assertTrue(res == 0);
	}
	// when the string contains numbers, letters etc. conse:cutive - this is  not always the case
	public int isPerm2(String data) {
		Set<Integer> present = new HashSet<>();
		char[] letters = data.toCharArray();
		int max = 0;
		int min = 0;
		for (int i = 0; i < letters.length; i++) {
			char ch = letters[i];
			int val = ch - '0';
			max = (max > val) ? max : val;
			min = (min < val) ? min : val;
			if (debug)
				System.err.println(String.format("Checking: %c", ch));
			present.add(ch - '0');
		}
		int size = max - min + 1;
		if (letters.length != size) {
			if (debug)
				System.err.println(String.format("Wrong array size: %d", letters.length));
			return 0;
		}
		// not enough
		if (present.size() != letters.length) {
			if (debug)
				System.err.println(String.format("Wrong set size: %d / %d ", present.size(), letters.length));
			return 0;
		} else if (debug)
			System.err.println(String.format("Size OK %d %s", present.size(), letters.length));
		if (present.size() != size || letters.length != size) {

			if (debug)
				System.err.println(String.format("Missing: %d elements / %d ", (size - present.size()), size));
			return 0;
		} else if (debug)
			System.err.println(String.format("All there %d %s", present.size(), Arrays.asList(present)));
		return 1;
	}


}
