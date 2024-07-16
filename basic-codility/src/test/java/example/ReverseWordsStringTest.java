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

public class ReverseWordsStringTest {

	private boolean debug = false;

	@Test
	public void test1() throws Exception {
		String data = "Welcome to Coding";
		String reversed = reverseWordOrder(data);
		System.err.println(reversed);
		String reversed2 = reverseWordLetters(data);
		System.err.println(reversed2);
		String reversed3 = reverseWordOrder(reversed2);
		System.err.println(reversed3);
		String[] letters = data.split("");
		invert(letters);
		String reversed4 = String.join("", letters);
		System.err.println(reversed4);
	}

	public String reverseWordOrder(String sentence) {
		String[] words = sentence.trim().split(" +");

		Collections.reverse(Arrays.asList(words));
		return String.join(" ", words);
	}

	// https://www.baeldung.com/java-invert-array
	public String reverseWordLetters(String sentence) {
		String[] words = sentence.trim().split(" +");
		for (int index = 0; index != words.length; index++) {
			char[] letters = words[index].toCharArray();
			invert(letters);
			words[index] = new String(letters);

		}
		return String.join(" ", words);
	}

	void invert(char[] data) {
		if (debug)
			System.err.println("orig: " + new String(data));
		for (int i = 0, j = data.length - 1; i <= j; i++, j--) {
			if (debug)
				System.err.println(String.format("Swap %c and %c", data[i], data[j]));
			char temp = data[i];
			data[i] = data[j];
			data[j] = temp;
		}
		if (debug)
			System.err.println("new: " + new String(data));

		return;
	}

	void invert(Object[] data) {
		for (int i = 0, j = data.length - 1; i <= j; i++, j--) {
			Object temp = data[i];
			data[i] = data[j];
			data[j] = temp;
		}

		return;
	}
}