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

	@Test(/* expected = ConcurrentModificationException.class */)
	// java.lang.AssertionError: Expected exception:
	// java.util.ConcurrentModificationException
	public void test2() throws InterruptedException {

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
}
