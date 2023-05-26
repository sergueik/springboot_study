package example;
/**
 * Copyright 2023 Serguei Kouzmine
 */

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import static org.hamcrest.Matchers.*;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.hasKey;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.core.StringContains.containsString;
import static org.junit.Assert.*;

import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;

import java.io.IOException;
import java.io.InputStream;

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

	@BeforeEach
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

	private static final String fileName = "application.yaml";

	@Test
	public void test6() throws Exception {
		String configuration = getFileContent(fileName);
		assertThat(configuration.split("\r?\n").length, is(7));
	}

	@Test
	public void test7() throws Exception {
		Properties properties = getApplicationProperties(PropertyParserTest.class);
		// System.err.println();
		assertThat(properties, hasKey("commandline"));
		String commandline = properties.getProperty("commandline");
		assertThat(commandline, containsString("https://www.google.com"));
		assertThat(commandline.split(" +").length, is(4));

	}

	@Test
	public void test9() {
		String value = "\"\"";

		if ((value != null) && (value != "") && (value != "\"\"")
				&& (value.replaceAll("[\"']", "") != "") && (!value.isEmpty())
				&& (value.length() != 0)) {

			System.err
					.println(String.format("Replacement \'%s\' %s %s %s %d [%s] [%s]",
							value.replaceAll("[\"']", ""),
							((value == "") ? "empty" : "non-empty"),
							((value == null) ? "empty" : "non-empty"),
							((value.replaceAll("[\"']", "") == "") ? "empty" : "non-empty"),
							value.length(), value.substring(0, 1), value.substring(1, 1)));
		}

	}

	@Test
	public void test8() throws Exception {
		List<String> tokens = Arrays
				.asList(getApplicationProperties(PropertyParserTest.class)
						.getProperty("commandline").split(" +"));
		List<String> configurations = Arrays
				.asList(getFileContent(fileName).split("\r?\n"));
		tokens.stream().forEach((String t) -> {
			String[] data = t.split("=");
			List<String> results = configurations.stream()
					.map((String line) -> replaceEntry(line, data[0], data[1]))
					.collect(Collectors.toList());
			String configuration = String.join("\n", results);
			System.err.println(configuration);
		});
	}

	private String replaceEntry(String payload, String name, String value) {

		p = Pattern.compile(expression1);
		// System.err.println(String.format("Pattern exression %s:\n",
		// p.toString()));
		String input = payload;
		m = p.matcher(input);

		if (m.find()) {
			assertThat(m.groupCount(), greaterThan(0));

			// System.err.println("group count: " + m.groupCount());
			// java.lang.IllegalArgumentException: No group with name <value>
			String captured1 = m.group(1).toString().trim();
			// System.err.println(String.format("group (1): \"%s\"", captured1));
			// assertThat(captured1, is(payload2));
			final String expression2 = String.format("^%s\\|\\|(\\w+)$", name);
			p = Pattern.compile(expression2);
			// System.err
			// .println(String.format("Pattern exression %s:\n", p.toString()));
			input = captured1;
			m = p.matcher(input);
			if (m.find()) {
				final String expression3 = String
						.format("\\{\\{\\* " + "%s\\|\\|(\\w+)" + " \\*\\}\\}.*$", name);
				if (value != null && !value.replaceAll("[\"']", "").isEmpty()) {
					// System.err.println(String.format("Replacement \"%s\"",
					// value.replaceAll("[\"']", "")));
					return payload.replaceAll(expression3, value);
				} else {
					// System.err.println("groups: " + m.groupCount());
					String captured2 = m.group(1).toString();
					// System.err.println(String.format("group(1): \"%s\"", captured2));
					// assertThat(captured2.trim(), is("default1"));
					return payload.replaceAll(expression3, captured2);
				}
			} else {
				// System.err.println("nothing found in " + input);
				return payload;
			}

		} else {
			// System.err.println("nothing found in " + input);
			return payload;
		}
	}

	public static String getFileContent(String fileName) {
		try {
			final InputStream stream = PropertyParserTest.class.getClassLoader()
					.getResourceAsStream(fileName);
			final byte[] bytes = new byte[stream.available()];
			stream.read(bytes);
			return new String(bytes, "UTF-8");
		} catch (IOException e) {
			throw new RuntimeException(fileName);
		}
	}

	// http://www.java2s.com/example/java/java.util/get-application-properties.html

	public static Properties getApplicationProperties(Class<?> c)
			throws IOException {
		Properties appProperties = new Properties();
		InputStream in = null;
		try {
			// NOTE: can use getFileContent here
			in = c.getClassLoader().getResourceAsStream("application.properties");
			System.err.println("Reading in " + in);
			appProperties.load(in);
			return appProperties;
		} finally {
			if (in != null) {
				in.close();
			}
		}
	}

}
