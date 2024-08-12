package example;
/**
 * Copyright 2024 Serguei Kouzmine
 */

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

//better matchers
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.any;
import static org.hamcrest.Matchers.hasEntry;
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

public class MutpltyPairTest {

	private boolean debug = true;

	@Ignore
	@Test
	// NOTE: With Arrays.asList will get UnsupportedOperationException
	public void test1() throws UnsupportedOperationException {
		final List<Integer> x = Arrays.asList(new Integer[] { 2, 3, 3, 4, 5, 6 });
		assertThat(MultiplyAdjustemtPair(x, 20), is(4));
		// assertTrue();
	}

	@Test
	public void test2() throws UnsupportedOperationException {
		final List<Integer> x = Arrays.asList(new Integer[] { 2, 3, 3, 4, 5, 6 });
		final List<Integer> y = new ArrayList<>();
		y.addAll(x);
		assertThat("test2", MultiplyAdjustemtPair(y, 20), is(3));
		// assertTrue();
	}

	@Test
	public void test3() throws UnsupportedOperationException {
		final List<Integer> x = Arrays.asList(new Integer[] { 6, 3, 4, 2, 3, 4, 5, 6 });
		final List<Integer> y = new ArrayList<>();
		y.addAll(x);
		assertThat("test3", MultiplyAdjustemtPair(y, 20), is(5));
		// assertTrue();
	}

	@Test
	public void test4() throws UnsupportedOperationException {
		final List<Integer> x = Arrays.asList(new Integer[] { 6, 3, 2, 3, 3, 4, 5, 6 });
		final List<Integer> y = new ArrayList<>();
		y.addAll(x);
		assertThat("test4", MultiplyAdjustemtPair2(y, 20), is(4));
		// assertTrue();
	}

	public int MultiplyAdjustemtPair(List<Integer> data, int threshold) throws UnsupportedOperationException {
		int size = data.size();
		int i = 0;
		while (i < size - 1) {
			int product = data.get(i) * data.get(i + 1);
			if (product <= threshold) {
				if (debug)
					System.err.println(String.format("index %d (%d, %d) replaced by (%d,)", i, data.get(i),
							data.get(i + 1), product));
				data.set(i, product);
				data.remove(i + 1);
				size = size - 1;
			} else {
				i++;

			}
		}
		return data.size();
	}

	public int MultiplyAdjustemtPair2(List<Integer> data, int threshold) throws UnsupportedOperationException {
		int size = data.size();
		int result = data.size();
		for (int j = 0; j != size - 2; j++) {
			int i = 0;
			while (i < size - 1) {
				int product = data.get(i) * data.get(i + 1);
				if (product <= threshold) {
					if (debug)
						System.err.println(String.format("index %d (%d, %d) replaced by (%d,)", i, data.get(i),
								data.get(i + 1), product));
					data.set(i, product);
					data.remove(i + 1);
					size = size - 1;
				} else {
					i++;

				}
			}
			if (result > data.size())
				result = data.size();
		}
		return result;
	}
}
