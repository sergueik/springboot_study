package example;
/**
 * Copyright 2024 Serguei Kouzmine
 */

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertFalse;

import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.ConcurrentModificationException;
import java.util.HashMap;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.hasKey;

// see also: https://www.baeldung.com/java-concurrentmodificationexception
public class MissingNumberTest {

	private boolean debug = true;
	//
	private final List<Integer> data = new ArrayList<>();
	// Arrays.asList(new Integer[] { 1, 2, 3, 4, 5, 6, 7, 8 });
	// java.lang.UnsupportedOperationException
	private int index;

	@Before
	public void before() {
		Integer[] numbers = new Integer[] { 0, 1, 2, 3, 4, 5, 6, 7, 8 };
		for (int cnt = 0; cnt != numbers.length; cnt++)
			data.add(numbers[cnt]);
		// index = 4;
		index = (int) (Math.random() * (data.size() - 1));
	}

	@Test
	public void test1() {

		
		if (debug)
			System.err.println("index: " + index);
		Integer number = data.remove(index);
		if (debug)
			System.err.println("number: " + number);
		Integer[] array = new Integer[data.size()];
		if (debug)
			System.err.println("numbers: " + Arrays.asList(data));

		data.toArray(array);
		int result = missingNumberHash(array);
		if (debug)
			System.err.println("result: " + result);
		assertThat(result, is(number));
	}

	@Test
	public void test2() {
		if (debug)
			System.err.println("index: " + index);
		Integer number = data.remove(index);
		if (debug)
			System.err.println("number: " + number);
		Integer[] array = new Integer[data.size()];
		if (debug)
			System.err.println("numbers: " + Arrays.asList(data));

		data.toArray(array);
		int result = missingNumberSum(array);
		if (debug)
			System.err.println("result: " + result);
		assertThat(result, is(number));
	}

	public int missingNumberSum(Integer[] data) {
		int sum = 0;
		if (debug)
			System.err.println("length: " + data.length);
		for (int i = 0; i < data.length + 1; i++) {
			sum += i;
		}
		if (debug)
			System.err.println("sum: " + sum);
		for (int i = 0; i < data.length; i++) {
			sum -= data[i];
		}
		if (debug)
			System.err.println("sum: " + sum);
		return sum;
	}

	public int missingNumberHash(Integer[] data) {
		int hash = 0;
		for (int i = 0; i < data.length + 1; i++) {
			hash ^= i;
		}
		if (debug)
			System.err.println("hash: " + hash);
		for (int i = 0; i < data.length; i++) {
			hash ^= data[i];
		}
		if (debug)
			System.err.println("hash: " + hash);
		return hash;
	}

}
