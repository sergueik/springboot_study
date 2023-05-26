package example;

import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.Assume;

import static org.hamcrest.Matchers.*;
import static org.junit.Assert.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class PropertyParserTest {

	private Pattern p;
	private Matcher m;
	private static final String name = "name1";
	private static final String payload1 = String
			.format("property1 = {{* %s||default1 *}}", name);
	private static final String payload2 = String.format("%s||default1", name);
	private static final String expression1 = "^.*\\{\\{\\*" + "(.+)"
			+ "\\*\\}\\}.*$";
	private static final String expression2 = String.format("^%s\\|\\|(\\w+)$",
			name);
	private static final String expression3 = String
			.format("\\{\\{\\* " + "%s\\|\\|(\\w+)" + " \\*\\}\\}.*$", name);

	@Before
	public void setup() {
	}

	@Test
	public void test1() throws Exception {
		// NOTE: without escaping {'s will get
		// java.util.regex.PatternSyntaxException: Illegal repetition
		// {{\*(?: *)name1||(\w+)(?: *)\*}}
		// see also:
		// https://stackoverflow.com/questions/8751482/patternsyntaxexception-illegal-repetition-when-using-regex-in-java

		p = Pattern.compile(expression1);
		System.err.println(String.format("Pattern exression %s:\n", p.toString()));
		String input = payload1;
		m = p.matcher(input);

		// NOTE: only one "find()" call will success, subsequent will fail
		assertThat(m.find(), is(true));
		// assertThat(m.find(), is(true));
		System.err
				.println(String.format("group(0): \"%s\"", m.group(0).toString()));

	}

	@Test
	public void test2() throws Exception {

		p = Pattern.compile(expression1);
		System.err.println(String.format("Pattern exression %s:\n", p.toString()));
		String input = payload1;
		m = p.matcher(input);

		if (m.find()) {
			assertThat(m.groupCount(), greaterThan(0));

			System.err.println("group count: " + m.groupCount());
			// java.lang.IllegalArgumentException: No group with name <value>
			String captured = m.group(1).toString();
			System.err.println(String.format("group (1): \"%s\"", captured));
			assertThat(captured.trim(), is(payload2));
		} else {
			System.err.println("nothing found in " + input);
		}

	}

	@Test
	public void test3() throws Exception {

		p = Pattern.compile(expression2);
		System.err.println(String.format("Pattern exression %s:\n", p.toString()));
		String input = payload2;
		m = p.matcher(input);
		if (m.find()) {
			System.err.println("groups: " + m.groupCount());
			String captured = m.group(1).toString();
			System.err.println(String.format("group(1): \"%s\"", captured));
			assertThat(captured.trim(), is("default1"));
		} else {
			System.err.println("nothing found in " + input);
		}
	}

	@Test
	public void test4() throws Exception {
		String result = replaceEntry(payload1, name, "value");
		assertThat(result, is("property1 = value"));
		System.err.println("test 4 result: " + result);
	}

	@Test
	public void test5() throws Exception {
		String result = replaceEntry(payload1, name, null);
		assertThat(result, is("property1 = default1"));
		System.err.println("test 4 result: " + result);
	}

	private String replaceEntry(String payload, String name, String value) {

		p = Pattern.compile(expression1);
		System.err.println(String.format("Pattern exression %s:\n", p.toString()));
		String input = payload;
		m = p.matcher(input);

		if (m.find()) {
			assertThat(m.groupCount(), greaterThan(0));

			System.err.println("group count: " + m.groupCount());
			// java.lang.IllegalArgumentException: No group with name <value>
			String captured1 = m.group(1).toString().trim();
			System.err.println(String.format("group (1): \"%s\"", captured1));
			assertThat(captured1, is(payload2));

			p = Pattern.compile(expression2);
			System.err
					.println(String.format("Pattern exression %s:\n", p.toString()));
			input = captured1;
			m = p.matcher(input);
			if (m.find()) {
				if (value != null) {
					return payload.replaceAll(expression3, value);
				} else {
					System.err.println("groups: " + m.groupCount());
					String captured2 = m.group(1).toString();
					System.err.println(String.format("group(1): \"%s\"", captured2));
					assertThat(captured2.trim(), is("default1"));
					return payload.replaceAll(expression3, captured2);
				}
			} else {
				System.err.println("nothing found in " + input);
				return payload;
			}

		} else {
			System.err.println("nothing found in " + input);
			return payload;
		}
	}
}
