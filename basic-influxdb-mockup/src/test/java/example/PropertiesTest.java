package example;
/**
 * Copyright 2022 Serguei Kouzmine
 */

import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.assertThat;

import org.junit.Before;
import org.junit.Test;

import example.utils.Utils;

public class PropertiesTest {

	private static final Utils utils = Utils.getInstance();
	private final static String payload = "test,foo=bar value=42.0 1655244130852723";
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
				"([-a-z0-9_]+)(?:,([-a-z0-9_=\"]+)*) (?:([-a-z0-9_]+=[-a-z.0-9_\"]+)(?:,[-a-z0-9_]+=[-a-z.0-9_\"]+)*) ([0-9]+)");
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

	@Test
	public void test6() {
		result = utils.parseLineProtocolLine(
				"testing,appid=BAZ,env=UAT,host=sergueik71,operation=send value=42.0 1655341280");
		
		assertThat(result, containsString("measurement=testing"));
	}

}
