package example;
/**
 * Copyright 2022 Serguei Kouzmine
 */

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import example.utils.Utils;

public class PropertiesTest {

	private static final Utils utils = Utils.getInstance();
	private static String payload = null;
	private static String result = null;

	@BeforeEach
	public void beforeTest() {
		utils.setDebug(true);
	}

	@Test
	public void test1() {
		payload = "test,foo=bar,baz=bam value=42.0 1655244130852723";
		result = utils.parseLineProtocolLine(payload);
		// NOTE: not the desired behavior
		assertThat(result, containsString("tag_set=baz=bam"));
	}

	private static String grammar = null;

	@Test
	public void test2() {
		payload = "test,foo=bar,baz=bam value=42.0 1655244130852723";
		grammar = "^([-a-z0-9_]+)((?:,(?:[-a-z0-9A-Z_=\"]+))*) (?:([-a-z.0-9_]+=[-a-z.0-9_\"]+)(?:,[-a-z0-9_]+=[-a-z.0-9_\"]+)*) ([0-9]+)$";
		result = utils.parseLineProtocolLine(payload, grammar);
		assertThat(result, containsString("tag_set=,foo=bar,baz=bam"));
	}

	@Test
	public void test3() {
		payload = "foo,bar=baz";
		result = utils.parseLineProtocolLine(payload,
				"([-a-z0-9_]+)((?:,[-a-z0-9_=\"]+)*)");
		assertThat(result, containsString("measurement=foo"));
	}

	@Test
	public void test4() {
		payload = "foo value=42";
		result = utils.parseLineProtocolLine(payload,
				"([-a-z0-9_]+)((?:,[-a-z0-9_=\"]+)*)");
		assertThat(result, containsString("measurement=foo"));
	}

	@Test
	public void test5() {
		result = utils.resolveTags(",foo=FOO,bar=BAR,baz=BAZ");

		for (String expression : new String[] { "tag_key=foo", "tag_value=FOO",
				"tag_key=bar", "tag_value=BAR", "tag_key=baz", "tag_value=BAZ" }) {
			assertThat(result, containsString(expression));
		}
	}

	@Test
	public void test6() {
		result = utils.resolveTags(",foo=FOO,bar=BAR,baz=BAZ");

		for (String expression : new String[] { "tag_key=foo", "tag_value=FOO",
				"tag_key=bar", "tag_value=BAR", "tag_key=baz", "tag_value=BAZ" }) {
			assertThat(result, containsString(expression));
		}
	}

	@Disabled
	@Test
	public void test7() {
		result = utils.parseLineProtocolLine(
				"testing,appid=BAZ,env=UAT,host=sergueik71,operation=send value=42.0 1655341280");

		for (String expression : new String[] { "measurement=testing",
				"tag_set=,appid=BAZ,env=UAT,host=sergueik71,operation=send" }) {
			assertThat(result, containsString(expression));
		}
	}

	@Test
	public void test8() {
		result = utils.resolveFields("foo=1 bar=2.0 baz=345",
				"([-a-z0-9_]+)=([-a-zA-Z0-9_]+)");
		for (String expression : new String[] { "field_key=foo", "field_value=1",
				"field_key=bar", "field_value=2", "field_key=baz", "field_value=345"

		}) {
			assertThat(result, containsString(expression));
		}
	}

}
