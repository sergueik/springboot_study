package example;
/**
 * Copyright 2022 Serguei Kouzmine
 */

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;

import java.util.Map;
import java.util.concurrent.TimeUnit;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import examle.model.Point;
import example.utils.LineProtocolParser;
import static org.hamcrest.CoreMatchers.notNullValue;

public class LineProtocolParserTest {

	private static final LineProtocolParser utils = LineProtocolParser
			.getInstance();
	private static String payload = null;
	private static String result = null;
	private static String grammar = null;

	@BeforeEach
	public void beforeTest() {
		utils.setDebug(true);
	}

	@Test
	public void test1() {
		payload = "test,foo=bar,baz=bam value=42.0,data=0 1655244130852723";
		result = utils.parseLineProtocolLine(payload);
		for (String expression : new String[] { "measurement=test",
				"tag_set=,foo=bar,baz=bam", "field_set=value=42.0,data=0",
				"timestamp=1655244130852723" }) {
			assertThat(result, containsString(expression));
		}
	}

	@Test
	public void test2() {
		result = utils.resolveTags(",foo=FOO,bar=BAR,baz=BAZ");

		for (String expression : new String[] { "tag_key=foo", "tag_value=FOO",
				"tag_key=bar", "tag_value=BAR", "tag_key=baz", "tag_value=BAZ" }) {
			assertThat(result, containsString(expression));
		}
	}

	@Test
	public void test3() {
		result = utils.resolveFields("foo=1,bar=2.0,baz=345");
		for (String expression : new String[] { "field_key=foo", "field_value=1",
				"field_key=bar", "field_value=2", "field_key=baz", "field_value=345"

		}) {
			assertThat(result, containsString(expression));
		}
	}

	@Test
	public void test4() {
		Point result = utils.extractPointFromLineProtocolLine(
				"test,foo=bar,baz=bam value=42.0,data=0 1655244130");
		// NOTE: second precision
		assertThat(result, notNullValue());
		assertThat(result instanceof Point, is(true));
		assertThat(result.getMeasurement(), is("test"));
		assertThat(result.getTime(), is((long) 1655244130));
		assertThat(result.getPrecision(), is(TimeUnit.SECONDS));
		assertThat(result.getFields() instanceof Map, is(true));
		Map<String, Object> fields = result.getFields();
		assertThat(fields.containsKey("value"), is(true));
		assertThat(fields.containsKey("data"), is(true));
		assertThat(result.getTags() instanceof Map, is(true));
		Map<String, String> tags = result.getTags();
		assertThat(tags.containsKey("foo"), is(true));
		assertThat(tags.containsKey("baz"), is(true));
	}

}
