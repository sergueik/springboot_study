package example;
/**
 * Copyright 2024 Serguei Kouzmine
 */

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.ConcurrentModificationException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;

import org.junit.Before;
import org.junit.Test;

// see also: https://www.baeldung.com/java-concurrentmodificationexception
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

	@Test
	// expected = ConcurrentModificationException.class
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
		assertThat(data.keySet().size(), is(2));
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

	@Test
	// origin: https://www.baeldung.com/java-synchronized-collections
	// see also:
	// https://docs.oracle.com/javase/tutorial/essential/concurrency/collections.html
	public void test6() throws InterruptedException {
		List<Integer> list = Collections.synchronizedList(new ArrayList<>());
		Runnable operation = () -> {
			list.addAll(Arrays.asList(1, 2, 3, 4, 5, 6));
		};

		Thread thread1 = new Thread(operation);
		Thread thread2 = new Thread(operation);
		thread1.start();
		thread2.start();
		thread1.join();
		thread2.join();

		assertThat(list.size(), is(12));
	}

}
