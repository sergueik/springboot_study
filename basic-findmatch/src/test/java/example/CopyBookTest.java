package example;

/**
 * Copyright 2026 Serguei Kouzmine
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

public class CopyBookTest {

	private Map<String, String> results;
	private FindMatch findMatch = new FindMatch();

	private static final String G_BRANCH = "(?<BRANCH>.{5})"; // Branch code: 5 chars
	private static final String G_DATE = "(?<TRANDATE>\\d{8})"; // Transaction date YYYYMMDD
	private static final String G_ACCOUNT = "(?<ACCOUNT>\\d{10,12})"; // Account number: 10 to 12 digits
	private static final String G_CODE = "(?<CODE>.{3})"; // Transaction code: 3 chars
	private static final String G_AMOUNT = "(?<AMOUNT>[0-9.]{9,12})"; // Amount: 0 to 12 chars including decimals
	private static final String G_CURRENCY = "(?<CURRENCY>[A-Z]{3})"; // Currency code: 3 chars

	private static final String COPYBOOK_REGEX = "^" + G_BRANCH + G_DATE + G_ACCOUNT + G_CODE + G_AMOUNT + G_CURRENCY
			+ "$";

	private static final String[] SAMPLE_DATA = {

		    // Built piece-by-piece (loose, readable copybook)
		    "BR001" +          // BRANCH
		    "20240130" +       // TRANDATE (yyyyMMdd)
		    "1234567890" +     // ACCOUNT (10 digits)
		    "DEP" +            // CODE
		    "000012345" +      // AMOUNT (9 digits, unscaled)
		    "USD",             // CURRENCY

		    // Flat, literal samples (more compact / realistic ingestion)
		    "BR002202312251111222233WDL000023456EUR",
		    "BR003202306151234000001DEP000000789USD"
		};

	@DisplayName("Verify healthy building the vanilla regex")
	@Test
	public void test1() {
		assertDoesNotThrow(() -> {
			for (String copybook : SAMPLE_DATA) {
				Pattern pattern = Pattern.compile(COPYBOOK_REGEX);
				Matcher matcher = pattern.matcher(copybook);
				matcher.find();
			}
		});

	}

	@DisplayName("Verify regex matches all simplified CopyBook samples")
	@Test
	public void test2() {
		assertDoesNotThrow(() -> {
			Pattern pattern = Pattern.compile(COPYBOOK_REGEX);

			for (String copybook : SAMPLE_DATA) {
				Matcher matcher = pattern.matcher(copybook);
				System.err.println(String.format("matching \"%s\"", copybook));
				System.err.println(String.format("regex: %s", COPYBOOK_REGEX));
				assertThat(matcher.matches(), is(true));
				// @formatter:off
				System.out.println(String.format(
				    "Matched record:%n" +
				    "  BRANCH   = %s%n" +
				    "  TRANDATE = %s%n" +
				    "  ACCOUNT  = %s%n" +
				    "  CODE     = %s%n" +
				    "  AMOUNT   = %s%n" +
				    "  CURRENCY = %s",
				    matcher.group("BRANCH"),
				    matcher.group("TRANDATE"),
				    matcher.group("ACCOUNT"),
				    matcher.group("CODE"),
				    matcher.group("AMOUNT"),
				    matcher.group("CURRENCY")
				));
			// @formatter:on

			}
		});
	}

	@DisplayName("Verify the resolve groups processing") 
	@Test
	public void test3() {
		List<String> groups = findMatch.resolveGroups(COPYBOOK_REGEX);
		assertThat(groups, notNullValue());
		assertThat(groups.size(), is(6));
		groups.stream().forEach(System.err::println);
	}

	@DisplayName("Verify processing by FindMatch")
	@Test
	public void test4() {

		for (String copybook : SAMPLE_DATA) {
			System.err.println(String.format("copybook: %s", copybook));
			results = findMatch.findMatch(copybook, COPYBOOK_REGEX);
			assertThat(results, notNullValue());
			assertThat(results.keySet().size(), is(6));
			for (String name : results.keySet()) {
				String result = results.get(name);
				System.err.println(String.format("%s: %s", name, result));
			}
		}
	}
}
