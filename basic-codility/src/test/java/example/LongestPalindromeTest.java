package example;
/**
 * Copyright 2024 Serguei Kouzmine
 */

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertFalse;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import java.util.Stack;
import java.util.*;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

public class LongestPalindromeTest {

	private final boolean debug = false;

	// @Ignore
	@Test
	public void test1() {
		String data = "abcba";
		boolean res = isPalindrome(data);
		assertTrue(res);
	}

	// @Ignore
	@Test
	public void test2() {
		String data = "abccba";
		boolean res = isPalindrome(data);
		assertTrue(res);
	}

	// @Ignore
	@Test
	public void test3() {
		String data = "abcda";
		boolean res = isPalindrome(data);
		assertFalse(res);

	}

	// @Ignore
	@Test
	public void test4() {
		String data = "abda";
		boolean res = isPalindrome(data);
		assertFalse(res);

	}

	// @Ignore
	@Test
	public void test5() {
		String data = "abcba";
		boolean res = isPalindrome2(data);
		assertTrue(res);
	}

	// @Ignore
	@Test
	public void test6() {
		String data = "abccba";
		boolean res = isPalindrome2(data);
		assertTrue(res);
	}

	// @Ignore
	@Test
	public void test7() {
		String data = "abcda";
		boolean res = isPalindrome2(data);
		assertFalse(res);

	}

	// @Ignore
	@Test
	public void test8() {
		String data = "abda";
		boolean res = isPalindrome2(data);
		assertFalse(res);

	}

	@Test
	public void test9() {
		String data = "abda";
		boolean res = isPalindrome4(data);
		assertFalse(res);
	}

	// @Ignore
	@Test
	public void test10() {
		String data = "I was driving a racecar with my mom at noon and saw the speed radar at level 0 of civic lane.";
		String result = longestPalindromeWord(data);
		System.err.println("Result: " + result);
		assertThat(result, is("racecar"));
		assertTrue(result.equals("racecar"));

	}

	@Test
	public void test12() {
		String data = "A man, a plan, a canal: Panama";
		boolean res = isPalindrome3(data);
		assertTrue(res);
	}

	@Test
	public void test13() {
		String data = "A man a plan a canal Panama";
		boolean res = isPalindrome3(data);
		assertTrue(res);
	}

	private boolean isPalindrome(String data) {
		String reverse = new StringBuilder(data).reverse().toString();
		for (int i = 0; i != data.length() / 2; i++) {
			if (data.charAt(i) != reverse.charAt(i)) {
				return false;
			}
		}
		return true;
	}

	private String longestPalindromeWord(String data) {
		String[] words = data.split(" ");
		Map<String, Integer> results = new HashMap<>();
		String result = null;
		int maxlength = 0;
		for (int i = 0; i != words.length - 1; i++) {
			if (isPalindrome(words[i])) {
				results.put(words[i], words[i].length());
				if (words[i].length() > maxlength) {
					maxlength = words[i].length();
					result = words[i];
				}
			}
		}
		return result;
	}

	private boolean isPalindrome2(String data) {

		char[] letters = data.toCharArray();
		for (int i = 0; i != (letters.length) / 2; i++) {
			int j = letters.length - 1 - i;
			char ch1 = letters[i];
			char ch2 = letters[j];
			if (debug)
				System.err.println(String.format("Scanning %c %c", ch1, ch2));
			if (ch1 != ch2) {
				if (debug)
					System.err.println("false");
				return false;
			}
		}
		return true;
	}

	private boolean isPalindrome3(String data) {
		if (data == null || data.isEmpty())
			return true;
		char[] letters = data.toCharArray();
		// NOTE: moving pointers together
		for (int i = 0, j = letters.length - 1; i <= j; i++, j--) {
			// TODO: switch to Character.isLetterOrDigit
			while (i <= j && !isLetterOrDigit(letters[i]))
				i++;
			while (i <= j && !isLetterOrDigit(letters[j]))
				j--;
			if (i > j)
				return true;
			if (Character.toLowerCase(letters[i]) != Character.toLowerCase(letters[j])) {
				System.err.println(String.format("Mismatch %c %c", letters[i], letters[j]));

				return false;
			}
		}
		return true;
	}

	private boolean isPalindrome4(String data) {
		String reverse = "";
		for (int i = 0; i < data.length(); i++) {
			reverse = reverse + data.charAt(i);
		}
		return (data.equals(reverse));
	}

	public static boolean isPalindromeNumber(int data) {
		int temp = data;
		int reverse = 0;
		while (temp > 0) {
			int reminder = temp % 10;
			reverse = reverse * 10 + reminder;
			temp = temp / 10;
		}
		return (data == reverse);
	}

	private boolean isLetterOrDigit(Character ch) {
		boolean status = (ch >= '0' && ch <= '9') || (ch >= 'A' && ch <= 'z');
		if (debug)
			System.err.println(String.format("%c %b", ch, status));
		return status;
	}

}
