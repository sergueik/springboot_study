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
// org.hamcrest.core.Is.is;
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

import org.apache.commons.lang3.StringUtils;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

// https://app.codility.com/programmers/lessons/13-fibonacci_numbers/ladder/
public class LadderFibonacciTest {
	// inspect
	// N = 1, 1
	// N = 2, 2
	// N = 3, 3
	// N = 4, 5
	// N = 5, 8
	//
	private boolean debug = true;

	@Test
	public void test1() {
		int size = 3;
		int result = fibonacciNR(size);
		if (debug)
			System.err.println("Result: " + result);
		assertThat(result, is(3));

	}

	@Test
	public void test2() {
		int size = 4;
		int result = fibonacciNR(size);
		if (debug)
			System.err.println("Result: " + result);
		assertThat(result, is(5));

	}

	@Test
	public void test3() {
		int size = 5;
		int result = fibonacciNR(size);
		if (debug)
			System.err.println("Result: " + result);
		assertThat(result, is(8));

	}

	@Test
	public void test4() {
		int size = 6;
		int result = fibonacciNR(size);
		if (debug)
			System.err.println("Result: " + result);
		assertThat(result, is(13));

	}

	@Test
	public void test5() {
		int size = 7;
		int result = fibonacciNR(size);
		if (debug)
			System.err.println("Result: " + result);
		assertThat(result, is(21));

	}

	@Test
	public void test6() {
		int[] A = new int[5];
		int[] B = new int[5];
		A[0] = 4;
		B[0] = 3;
		A[1] = 4;
		B[1] = 2;
		A[2] = 5;
		B[2] = 4;
		A[3] = 5;
		B[3] = 3;
		A[4] = 1;
		B[4] = 1;
		int[] results = fibonacciMultiNR(A, B);
		if (debug)
			System.err.println("Result: " + Arrays.asList(results));
		// https://stackoverflow.com/questions/54439629/hamcrest-matcher-to-compare-two-arrays
		assertThat(results, is(new Integer[] { 5, 1, 8, 0, 1 }));

	}

	@Test
	public void test7() {
		int[] A = new int[11];
		int[] B = new int[11];
		A[0] = 4;
		B[0] = 3;
		A[1] = 4;
		B[1] = 2;
		A[2] = 5;
		B[2] = 4;
		A[3] = 5;
		B[3] = 3;
		A[4] = 1;
		B[4] = 1;
		A[5] = 11;
		B[5] = 8;
		A[6] = 4;
		B[6] = 3;
		A[7] = 4;
		B[7] = 3;
		A[8] = 4;
		B[8] = 3;
		A[9] = 4;
		B[9] = 3;
		A[10] = 4;
		B[10] = 3;

		int[] results = fibonacciMultiNR(A, B);
		if (debug)
			System.err.println("Result: " + Arrays.asList(results));
		// https://stackoverflow.com/questions/54439629/hamcrest-matcher-to-compare-two-arrays
		assertThat(results, is(new int[] { 5, 1, 8, 0, 1, 144, 5, 5, 5, 5, 5 }));

	}

	private int fibonacciNR(int size) {
		int[] fib = new int[size + 1];
		// https://en.wikipedia.org/wiki/Fibonacci_sequence
		fib[0] = 1;
		fib[1] = 1;
		fib[2] = 2;
		if (size > 2) {
			for (int k = 3; k <= size; k++)
				fib[k] = fib[k - 1] + fib[k - 2];
		}
		// The array is traversed only once so time complexity is O(N)\
		return fib[size];
	}

	private int[] fibonacciMultiNR(int[] rungs, int[] scales) {
		int size = rungs.length;

		int max = 0;
		for (int i = 0; i < size; i++) {
			max = Math.max(rungs[i], max);
		}
		long[] fib = new long[size + 1];
		fib[0] = 1L;
		fib[1] = 1L;
		fib[2] = 2L;
		if (size > 2) {
			for (int k = 3; k <= size; k++)
				fib[k] = fib[k - 1] + fib[k - 2];
		}
		int[] results = new int[size];
		for (int i = 0; i < size; i++) {
			results[i] = (int) (fib[rungs[i]] % (01 << scales[i]));
		}
		return results;

	}

}
