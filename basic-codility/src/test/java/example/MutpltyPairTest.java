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

// Smallest array that can be obtained by replacing adjacent pairs with their products
// NOTE: different exit condition:
// If all array elements become equal,
// see also:
// https://www.geeksforgeeks.org/smallest-array-that-can-be-obtained-by-replacing-adjacent-pairs-with-their-products/
// see also: https://stackoverflow.com/questions/76026322/minimize-array-elements-by-multiplying-adjacent-elements-only-if-the-product-is
// https://leetcode.ca/2023-10-09-2892-Minimizing-Array-After-Replacing-Pairs-With-Their-Product/
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

	@Ignore
	@Test
	public void test2() throws UnsupportedOperationException {
		final List<Integer> x = Arrays.asList(new Integer[] { 2, 3, 3, 4, 5, 6 });
		final List<Integer> y = new ArrayList<>();
		System.err.println("test2");
		y.addAll(x);
		assertThat("test2", MultiplyAdjustemtPair(y, 20), is(3));
		// assertTrue();
	}

	@Ignore
	@Test
	public void test3() {
		final List<Integer> x = Arrays.asList(new Integer[] { 6, 3, 4, 2, 3, 4, 5, 6 });
		final List<Integer> y = new ArrayList<>();
		y.addAll(x);
		System.err.println("test3");
		assertThat("test3", MultiplyAdjustemtPair(y, 20), is(5));
		// assertTrue();
	}

	@Ignore
	@Test
	public void test4() {
		final List<Integer> x = Arrays.asList(new Integer[] { 6, 3, 2, 3, 3, 4, 5, 6 });
		final List<Integer> y = new ArrayList<>();
		y.addAll(x);
		System.err.println("test4");

		assertThat("test4", MultiplyAdjustemtPair2(y, 20), is(4));
		// assertTrue();
	}

	@Ignore
	@Test
	public void test5() {
		final List<Integer> x = Arrays.asList(new Integer[] { 7, 3, 1, 1, 1, 1, 1, 1, 1 });
		final List<Integer> y = new ArrayList<>();
		y.addAll(x);
		System.err.println("test5");
		assertThat("test5", MultiplyAdjustemtPair2(y, 20), is(2));

		assertThat("test5", MultiplyAdjustemtPair(y, 20), is(y.size()));
		assertThat("test5", MultiplyAdjustemtPair3(y, 20), is(2));
		// assertTrue();
	}

	@Test
	public void test6() {
		final List<Integer> x = Arrays.asList(new Integer[] { 7, 3, 1, 1, 1, 1, 1, 1, 1 });
		final List<Integer> y = new ArrayList<>();
		y.addAll(x);
		System.err.println("test6");
		assertThat("test6", MultiplyAdjustemtPair(y, 20), is(y.size()));
		System.err.println("test6: " + y);
		// assertTrue();
	}

	public int MultiplyAdjustemtPair(List<Integer> data, int threshold) throws UnsupportedOperationException {
		int size = data.size();
		int i = 0;
		while (i < size - 1) {
			int product = data.get(i) * data.get(i + 1);
			if (product <= threshold) {
				if (debug)
					System.err.println(String.format("index %d (%d, %d) replaced by (%d,) size= %d", i, data.get(i),
							data.get(i + 1), product, size));
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
		List<Integer> dataCopy = new ArrayList<>();
		int result = data.size();
		for (int j = 0; j < size - 2; j++) {
			dataCopy.clear();
			dataCopy.addAll(data);
			int i = j;
			while (i < size - 1) {
				if (debug)
					System.err.println(String.format("index i= %d j= %d size = %d", i, j, size));
				int product = data.get(i) * data.get(i + 1);
				if (product <= threshold) {
					if (debug)
						System.err.println(String.format("index %d (%d, %d) replaced by (%d,)", i, data.get(i),
								data.get(i + 1), product));
					data.set(i, product);
					data.remove(i + 1);
					size = size - 1;
					if (debug)
						System.err.println("...");
				} else {
					i++;
					if (debug)
						System.err.println(String.format("increment index i=%d j=%d size = %d", i, j, size));
				}
			}
			if (result > data.size() + j)
				result = data.size() + j;
		}
		return result;
	}

	public int MultiplyAdjustemtPair3(List<Integer> data, int threshold) {
		int result = 1;
		long y = data.get(0);
		for (int i = 1; i < data.size(); ++i) {
			int x = data.get(i);
			if (x == 0) {
				return 1;
			}
			if (x * y <= threshold) {
				y *= x;
			} else {
				y = x;
				++result;
			}
		}
		return result;
	}

	public int MultiplyAdjustemtPair3(int[] nums, int k) {
		int result = 1;
		long y = nums[0];
		for (int i = 1; i < nums.length; ++i) {
			int x = nums[i];
			if (x == 0) {
				return 1;
			}
			if (x * y <= k) {
				y *= x;
			} else {
				y = x;
				++result;
			}
		}
		return result;
	}
}