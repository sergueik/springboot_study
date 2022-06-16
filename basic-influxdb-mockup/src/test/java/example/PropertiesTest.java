package example;
/**
 * Copyright 2022 Serguei Kouzmine
 */

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.Matchers.containsInAnyOrder;
// NOTE: need to provide all entries
import static org.hamcrest.Matchers.hasItems;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.core.AnyOf.anyOf;

import static org.junit.Assert.assertThat;

import java.io.InputStream;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.io.IOUtils;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

public class PropertiesTest {

	private static final Utils utils = Utils.getInstance();
	private final static String payload = "test,foo=bar value=42 1655244130852723";
	private static String result = null;

	@Before
	public void beforeTest() {
		utils.setDebug(true);
	}

	@Test
	public void test1() {
		result = utils.parseLineProtocolLine(payload);
		assertThat(result, containsString("measurement=test"));
	}

	@Test
	public void test2() {
		result = utils.parseLineProtocolLine(payload,
				"([-a-z0-9_]+)(?:,([-a-z0-9_=\"]+)*) (?:([-a-z0-9_]+=[-a-z0-9_\"]+)(?:,[-a-z0-9_]+=[-a-z0-9_\"]+)*) ([0-9]+)");
		assertThat(result, containsString("measurement=test"));
	}

	@Test
	public void test3() {
		result = utils.parseLineProtocolLine("foo,bar=baz",
				"([-a-z0-9_]+)((?:,[-a-z0-9_=\"]+)*)");
		assertThat(result, containsString("measurement=foo"));
	}

	@Test
	public void test4() {
		result = utils.parseLineProtocolLine("foo value=42",
				"([-a-z0-9_]+)((?:,[-a-z0-9_=\"]+)*)");
		assertThat(result, containsString("measurement=foo"));
	}

	@Test
	public void test5() {
		result = utils.resolveTags("foo=FOO,bar=BAR,baz=BAZ",
				"([-a-z0-9_]+)=([-a-zA-Z0-9_]+)(?:,([-a-zA-Z0-9_=\"]+)*)");
		assertThat(result, containsString("tag_key=foo"));
	}

}
