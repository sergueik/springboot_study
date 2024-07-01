package example;
/**
 * Copyright 2024 Serguei Kouzmine
 */

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertFalse;

import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
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
// 
public class ConcurrentModificationExceptionTest {

	private boolean debug = true;
	private final List<Integer> integers = Arrays.asList(new Integer[] { 1, 2, 3 });
	private Map<Integer, Object> data = null;

	@Before
	public void before() {
	}

	// @Ignore
	@Test(expected = ConcurrentModificationException.class)
	public void test1() throws InterruptedException {

		data = new HashMap<>();
		for (Integer i : integers)
			// remove one key
			data.put(i, new Object());
		for (Integer i : data.keySet()) {
			if (i != 2)
				continue;
			data.remove(i);
		}
	}

	// origin:
	// https://www.baeldung.com/java-synchronizedmap-vs-concurrenthashmap
	@Test(expected = ConcurrentModificationException.class)
	public void test2() {
		data = new HashMap<>();
		data.put(1, new Object());
		data.put(2, new Object());
		Map<Integer, Object> synchronizedMap = Collections.synchronizedMap(data);
		Iterator<Entry<Integer, Object>> iterator = synchronizedMap.entrySet().iterator();
		while (iterator.hasNext()) {
			try {
				synchronizedMap.put(3, new Object());

				iterator.next();
			} catch (ConcurrentModificationException e) {
				System.err.println("Exception " + e.toString());
				throw e;
			}
		}
	}

	@Test(/* expected = ConcurrentModificationException.class */)
	// java.lang.AssertionError: Expected exception:
	// java.util.ConcurrentModificationException
	public void test3() throws InterruptedException {

		data = new ConcurrentHashMap<>();

		for (Integer i : integers)
			data.put(i, new Object());
		// remove one key
		for (Integer i : data.keySet()) {
			if (i != 2)
				continue;
			data.remove(i);
		}
		assertThat(data.keySet().size(), is(new Integer(2)));
	}

	@Test(expected = NullPointerException.class)
	public void test4() {
		Map<String, Integer> map = new ConcurrentHashMap<>();
		map.put(null, 1);
	}

	@Test
	public void test5() {
		Map<String, Integer> map = Collections.synchronizedMap(new HashMap<>());
		map.put(null, 1);
		assertTrue(map.get(null).equals(1));
	}
}
