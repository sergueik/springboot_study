package example;

/**
 * Copyright 2021,2026 Serguei Kouzmine
 */
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.core.IsNot.not;
import static org.hamcrest.CoreMatchers.notNullValue;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;

import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import example.FindMatch;

public class BasicTest {
	private final String backgroundColorAttribute = "(100,150,250)";

	private final String patternExression = "\\(\\s*(?<red>\\d+),\\s*(?<green>\\d+),\\s*(?<blue>\\d+)\\)";
	private Pattern pattern;
	private Matcher matcher;
	private Map<String, String> results;
	int red = 0, green = 0, blue = 0;
	private FindMatch findMatch = new FindMatch();

	private final String tagMatcher = "(?:<(?<result>[^>]+)>)";

	@BeforeEach
	public void setup() {
	}

	@DisplayName("Verify that the straight-ahead")
	@Test
	public void test1() {

		pattern = Pattern.compile("\\(\\s*(\\d+),\\s*(\\d+),\\s*(\\d+)\\)");
		matcher = pattern.matcher(backgroundColorAttribute);
		if (matcher.find()) {
			System.err.println("backgroundColorAttribute:" + backgroundColorAttribute);

			pattern = Pattern.compile(patternExression);
			matcher = pattern.matcher(backgroundColorAttribute);
			if (matcher.find()) {
				red = Integer.parseInt(matcher.group("red").toString());
				green = Integer.parseInt(matcher.group("green").toString());
				blue = Integer.parseInt(matcher.group("blue").toString());
				assertThat(green, greaterThan(128));
				System.err.println("green:" + green);
			}

		}
	}

	@DisplayName("Verify the find match processing, long argument list")
	@Test
	public void test2() {

		results = findMatch.findMatch(backgroundColorAttribute, patternExression,
				Arrays.asList(new String[] { "red", "green", "blue" }));
		assertThat(results, notNullValue());
		assertThat(results.keySet().size(), is(3));
		for (String name : results.keySet()) {
			int result = Integer.parseInt(results.get(name).toString());
			System.err.println(String.format("%s: %d", name, result));
		}
	}

	@DisplayName("Verify the find match processing, short argument list")
	@Test
	public void test3() {

		results = findMatch.findMatch(backgroundColorAttribute, patternExression);
		assertThat(results, notNullValue());
		assertThat(results.keySet().size(), is(3));
		for (String name : results.keySet()) {
			int result = Integer.parseInt(results.get(name).toString());
			System.err.println(String.format("%s: %d", name, result));
		}
	}

	@DisplayName("Verify the resolve groups processing")
	@Test
	public void test4() {
		List<String> groups = findMatch.resolveGroups(patternExression);
		assertThat(groups, notNullValue());
		assertThat(groups.size(), is(3));
	}

}
