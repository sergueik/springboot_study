package example;

/**
 * Copyright 2014,2021,2026 Serguei Kouzmine
 */
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.core.IsNot.not;
import static org.hamcrest.CoreMatchers.notNullValue;
import java.util.regex.PatternSyntaxException;

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

// import example.FindMatch;

public class CopyBookTest {
	private final String backgroundColorAttribute = "(100,150,250)";

	private final String patternExression = "\\(\\s*(?<red>\\d+),\\s*(?<green>\\d+),\\s*(?<blue>\\d+)\\)";
	private Pattern pattern;
	private Matcher matcher;
	private Map<String, String> results;
	int red = 0, green = 0, blue = 0;
	// private FindMatch findMatch = new FindMatch();

	private final String tagMatcher = "(?:<(?<result>[^>]+)>)";
	private final String copybook2 = "BR001" + "T12345" + "0012" + "20240130" + "103015" + "123456789012" + "DEP "
			+ "0000001234567" + "USD" + String.format("%-30s", "ATM WITHDRAWAL") + "00" + "APR123";
	private final String copybook = "BR001T123450012202401301030151234567890123DEP 00000012345.67USDATM WITHDRAWAL            00APR123";

	@BeforeEach
	public void setup() {
	}

	// NOTE:
	// Java 8 or earlier support letters and digits only in group names, not underscores. 
	// TT_BRANCH_ID works in Java 11+, fails in Java 8
	// NOTE: Java 11 still does not allow underscores in named groups
	private static final String G_BRANCH = "(?<TT_BRANCH_ID>.{5})".replaceAll("_", ""); // PIC X(05)
	private static final String G_TELLER = "(?<TT_TELLER_ID>.{6})".replaceAll("_", ""); // PIC X(06)
	private static final String G_TERMINAL = "(?<TT_TERMINAL_ID>.{4})".replaceAll("_", ""); // PIC X(04)
	private static final String G_DATE = "(?<TT_TRAN_DATE>\\d{8})".replaceAll("_", ""); // PIC 9(08)
	private static final String G_TIME = "(?<TT_TRAN_TIME>\\d{6})".replaceAll("_", ""); // PIC 9(06)
	private static final String G_ACCOUNT = "(?<TT_ACCOUNT_NUMBER>\\d{12}).replaceAll(\"_\", \"\")"; // PIC 9(12)
	private static final String G_CODE = "(?<TT_TRAN_CODE>.{4})".replaceAll("_", ""); // PIC X(04)
	private static final String G_AMOUNT = "(?<TT_AMOUNT>[+-]?\\d{13})".replaceAll("_", ""); // PIC S9(11)V99
	private static final String G_CURRENCY = "(?<TT_CURRENCY>[A-Z]{3})".replaceAll("_", ""); // PIC X(03)
	private static final String G_DESC = "(?<TT_DESCRIPTION>.{30})".replaceAll("_", ""); // PIC X(30)
	private static final String G_RESPONSE = "(?<TT_RESPONSE_CODE>.{2})".replaceAll("_", ""); // PIC X(02)
	private static final String G_APPROVAL = "(?<TT_APPROVAL_CODE>.{6})".replaceAll("_", ""); // PIC X(06)

	public static String[] testData() {
		return new String[] { G_BRANCH, G_BRANCH + G_TELLER, G_BRANCH + G_TELLER + G_TERMINAL,
				G_BRANCH + G_TELLER + G_TERMINAL + G_DATE, G_BRANCH + G_TELLER + G_TERMINAL + G_DATE + G_TIME,
				G_BRANCH + G_TELLER + G_TERMINAL + G_DATE + G_TIME + G_ACCOUNT,
				G_BRANCH + G_TELLER + G_TERMINAL + G_DATE + G_TIME + G_ACCOUNT + G_CODE,
				G_BRANCH + G_TELLER + G_TERMINAL + G_DATE + G_TIME + G_ACCOUNT + G_CODE + G_AMOUNT,
				G_BRANCH + G_TELLER + G_TERMINAL + G_DATE + G_TIME + G_ACCOUNT + G_CODE + G_AMOUNT + G_CURRENCY,
				G_BRANCH + G_TELLER + G_TERMINAL + G_DATE + G_TIME + G_ACCOUNT + G_CODE + G_AMOUNT + G_CURRENCY
						+ G_DESC,
				G_BRANCH + G_TELLER + G_TERMINAL + G_DATE + G_TIME + G_ACCOUNT + G_CODE + G_AMOUNT + G_CURRENCY + G_DESC
						+ G_RESPONSE,
				G_BRANCH + G_TELLER + G_TERMINAL + G_DATE + G_TIME + G_ACCOUNT + G_CODE + G_AMOUNT + G_CURRENCY + G_DESC
						+ G_RESPONSE + G_APPROVAL, };
	}

	// @formatter:off
	public static Stream<String> testDataStream() {
		return Stream.of(G_BRANCH, G_BRANCH + G_TELLER, G_BRANCH + G_TELLER + G_TERMINAL,
				G_BRANCH + G_TELLER + G_TERMINAL + G_DATE, G_BRANCH + G_TELLER + G_TERMINAL + G_DATE + G_TIME,
				G_BRANCH + G_TELLER + G_TERMINAL + G_DATE + G_TIME + G_ACCOUNT,
				G_BRANCH + G_TELLER + G_TERMINAL + G_DATE + G_TIME + G_ACCOUNT + G_CODE,
				G_BRANCH + G_TELLER + G_TERMINAL + G_DATE + G_TIME + G_ACCOUNT + G_CODE + G_AMOUNT,
				G_BRANCH + G_TELLER + G_TERMINAL + G_DATE + G_TIME + G_ACCOUNT + G_CODE + G_AMOUNT + G_CURRENCY,
				G_BRANCH + G_TELLER + G_TERMINAL + G_DATE + G_TIME + G_ACCOUNT + G_CODE + G_AMOUNT + G_CURRENCY
						+ G_DESC,
				G_BRANCH + G_TELLER + G_TERMINAL + G_DATE + G_TIME + G_ACCOUNT + G_CODE + G_AMOUNT + G_CURRENCY + G_DESC
						+ G_RESPONSE,
				G_BRANCH + G_TELLER + G_TERMINAL + G_DATE + G_TIME + G_ACCOUNT + G_CODE + G_AMOUNT + G_CURRENCY + G_DESC
						+ G_RESPONSE + G_APPROVAL);
	}
	// @formatter:on

	@Disabled
	@DisplayName("Verify building of the regex")
	@ParameterizedTest
	@MethodSource("testData")
	// java.util.regex.PatternSyntaxException
	void test1(final String data) {
		System.err.println(String.format("testing %s", data));
		assertDoesNotThrow(() -> {
			Pattern.compile("^" + data + "$");
		});
	}

	@DisplayName("Verify running of the regex")
	@ParameterizedTest
	@MethodSource("testDataStream")
	public void test2(String data) {
		// match trailing non-captured data
		System.err.println(String.format("testing %s", data));
		assertDoesNotThrow(() -> {
			Pattern pattern = Pattern.compile("^" + data + ".*$");
			Matcher matcher = pattern.matcher(copybook);
			matcher.find();
		});
	}

	@DisplayName("Verify building of the regex with error reporting")
	@ParameterizedTest
	@MethodSource("testData")
	void test3(final String data) {
		System.err.println(String.format("testing length=%d", data.length()));
		try {
			Pattern.compile("^" + data + "$");
		} catch (PatternSyntaxException e) {
			// Report more information
			System.err.println("PatternSyntaxException caught!");
			System.err.println("Input length: " + data.length());
			System.err
					.println("First 50 chars (or full if smaller): " + data.substring(0, Math.min(50, data.length())));
			System.err.println("Exception description: " + e.getDescription());
			System.err.println("Exception index: " + e.getIndex());
			System.err.println("Exception pattern: " + e.getPattern());
			// Re-throw if you still want the test to fail
			throw e;
		}
	}
}
