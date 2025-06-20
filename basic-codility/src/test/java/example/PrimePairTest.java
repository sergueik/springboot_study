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

public class PrimePairTest {

	private boolean debug = true;

	// @Ignore
	@Test
	public void test1() {
		final int x = 10837;
		assertThat(isPrime(x), is(true));
		// assertTrue();
	}

	@Test
	public void test2() {
		final int x = 4;
		boolean y = isPrime(x);
		assertThat(y, is(false));
		// assertTrue();
	}

	public boolean isPrime(int n) {
		if (debug)
			System.err.println(String.format("testing %d", n));
		if (n == 2 || n == 3)
			return true;
		// NOTE, cut off with ceil
		for (int i = 2; i < Math.sqrt(n) + 1; i++) {
			if (n % i == 0) {
				if (debug)
					System.err.println(String.format("%d divided by %d", n, i));
				return false;
			}
		}
		return true;
	}
}
