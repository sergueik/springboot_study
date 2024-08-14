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

//https://stackoverflow.com/questions/22685930/implementing-two-interfaces-with-two-default-methods-of-the-same-signature-in-ja

public class OverrideDuplicateDefaultTest {

	// NOTE: without overriding colliding methods,
	// class won't compile
	// Duplicate default methods named defaultMethod with the parameters () and
	// () are inherited from the types Interface2 and Interface1
	//
	public static class ClassOverrideDuplicateDefault implements Interface1, Interface2 {
		public String someMethod1() {
			return "";

		}

		public String someMethod2() {
			return "";

		}

		// quick fix: override the default method in one of the interfaces
		@Override
		public String defaultMethod() {
			// return Interface1.super.defaultMethod();
			// can use either or both interface default methods
			return Interface1.super.defaultMethod() + Interface2.super.defaultMethod();
		}
	}

	private boolean debug = true;

	// @Ignore
	@Test
	public void test1() {
		String data = new ClassOverrideDuplicateDefault().defaultMethod();
		assertThat(data, is("Interface1" + "Interface2"));
		// assertTrue();
	}
}
