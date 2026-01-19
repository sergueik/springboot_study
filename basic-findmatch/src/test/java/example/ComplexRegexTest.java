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

import java.lang.IllegalStateException;
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

import example.FindMatch;

public class ComplexRegexTest {
	private final String backgroundColorAttribute = "(100,150,250)";

	private final String patternExression = "\\(\\s*(?<red>\\d+),\\s*(?<green>\\d+),\\s*(?<blue>\\d+)\\)";
	private Pattern pattern;
	private Matcher matcher;
	private Map<String, String> results;
	private FindMatch findMatch = new FindMatch();

	private final String tagMatcher = "(?:<(?<result>[^>]+)>)";
	private final String copybook2 = "BR001" + "T12345" + "0012" + "20240130" + "103015" + "123456789012" + "DEP "
			+ "0000001234567" + "USD" + String.format("%-30s", "ATM WITHDRAWAL") + "00" + "APR123";
	private final String copybook = "BR001T123450012202401301030151234567890123DEP 00000012345.67USDATM WITHDRAWAL            00APR123";

	@BeforeEach
	public void setup() {
	}

	// NOTE:
	// Java 11 or earlier does not allow underscores in named groups

	private static final String G_BRANCH = "(?<TTBRANCHID>.{5})"; // PIC X(05)
	private static final String G_TELLER = "(?<TTTELLERID>.{6})"; // PIC X(06)
	private static final String G_TERMINAL = "(?<TTTERMINALID>.{4})"; // PIC X(04)
	private static final String G_DATE = "(?<TTTRANDATE>\\d{8})"; // PIC 9(08)
	private static final String G_TIME = "(?<TTTRANTIME>\\d{6})"; // PIC 9(06)
	private static final String G_ACCOUNT = "(?<TTACCOUNTNUMBER>\\d{12})"; // PIC 9(12)
	private static final String G_CODE = "(?<TTTRANCODE>.{4})"; // PIC X(04)
	private static final String G_AMOUNT = "(?<TTAMOUNT>[+-]?\\d{13})"; // PIC S9(11)V99
	private static final String G_CURRENCY = "(?<TTCURRENCY>[A-Z]{3})"; // PIC X(03)
	private static final String G_DESC = "(?<TTDESCRIPTION>.{30})"; // PIC X(30)
	private static final String G_RESPONSE = "(?<TTRESPONSECODE>.{2})"; // PIC X(02)
	private static final String G_APPROVAL = "(?<TTAPPROVALCODE>.{6})"; // PIC X(06)

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

	@DisplayName("Verify if pattern constructor hits a hard JVM regex limit when building of the regex with error reporting")
	@ParameterizedTest
	@MethodSource("testDataStream")
	void test(final String data) {
		System.err.println(String.format("testing length=%d", data.length()));
		try {
			Pattern.compile("^" + data + "$");
		} catch (PatternSyntaxException e) {
			// Report more information
			System.err.println("PatternSyntaxException caught! " + "Input length=" + data.length()
					+ ", First 50 chars=\"" + data.substring(0, Math.min(50, data.length())) + "\"" + ", Description=\""
					+ e.getDescription() + "\"" + ", Index=" + e.getIndex() + ", Pattern=\"" + e.getPattern() + "\"");
		} catch (IllegalStateException e) {
			// Report more information
			System.err.println("PatternSyntaxException caught! " + "Input length=" + data.length()
					+ ", First 50 chars=\"" + data.substring(0, Math.min(50, data.length())) + "\""
					+ ", Description=\"NONE\"" + ", Pattern=\"NONE\"");
		}
	}
}
