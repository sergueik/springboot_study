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
import java.util.stream.Collectors;
import java.util.stream.Stream;
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

public class ConversionTest {

	private boolean debug = true;

	@Test
	public void test1() {
		int a = 5;
		int b = 7;
		String result = String.format("a + b = " + a + b);
		String expected = "a + b = " + Integer.toString(a) + Integer.toString(b);
		if (debug) {
			System.err.println(result);
			System.err.println(expected);
			System.err.println(result.equals(expected));
			System.err.println(result.contains(expected));
		}
		assertThat(result, is(expected));
		// assertTrue(result.contains(expected));
	}

	@Test
	public void test2() {
		int a = 5;
		int b = 7;
		String result = String.format("a + b = " + (a + b));
		String expected = "a + b = " + Integer.toString(a + b);
		if (debug) {
			System.err.println(result);
			System.err.println(expected);
			System.err.println(result.equals(expected));
			System.err.println(result.contains(expected));
		}
		assertThat(result, is(expected));
		// assertTrue(result.contains(expected));
	}
}
