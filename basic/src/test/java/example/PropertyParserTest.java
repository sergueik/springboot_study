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
	private final String name = "name1";
	private final String payload1 = String
			.format("property1 = {{* %s||default1 *}}", name);
	private final String payload2 = String.format("%s||default1", name);

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
		String expression = "^.*\\{\\{\\*" + "(.+)" + "\\*\\}\\}.*$";
		p = Pattern.compile(expression);
		System.err.println(String.format("Pattern exression %s:\n", p.toString()));
		m = p.matcher(payload1);
		// NOTE: only one "find()" call will success, subsequent will fail
		assertThat(m.find(), is(true));
		// assertThat(m.find(), is(true));
		System.err
				.println(String.format("group(0): \"%s\"", m.group(0).toString()));

	}

	@Test
	public void test2() throws Exception {
		String expression = "^.*\\{\\{\\*" + "(.+)" + "\\*\\}\\}.*$";
		p = Pattern.compile(expression);
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

		String expression = String.format("^%s\\|\\|(\\w+)$", name);
		p = Pattern.compile(expression);
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
}
