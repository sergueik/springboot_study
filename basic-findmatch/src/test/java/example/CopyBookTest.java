package example;

/**
 * Copyright 2026 Serguei Kouzmine
 */

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.jayway.jsonpath.JsonPath;
import org.junit.jupiter.api.Test;

import java.util.Map;
import java.util.Set;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.jayway.jsonpath.JsonPath;
import com.jayway.jsonpath.PathNotFoundException;

import org.junit.jupiter.api.Test;

import java.util.Map;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.core.IsNot.not;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import java.lang.reflect.Field;

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

	private static final String regexString = "^" + G_BRANCH + G_DATE + G_ACCOUNT + G_CODE + G_AMOUNT + G_CURRENCY
			+ "$";

	private static final String[] rows = {

			// Built piece-by-piece (loose, readable copyBook)
			"BR001" + // BRANCH
					"20240130" + // TRANDATE (yyyyMMdd)
					"1234567890" + // ACCOUNT (10 digits)
					"DEP" + // CODE
					"000012345" + // AMOUNT (9 digits, unscaled)
					"USD", // CURRENCY

			// Flat, literal samples (more compact / realistic ingestion)
			"BR002202312251111222233WDL000023456EUR", "BR003202306151234000001DEP000000789USD" };

	@DisplayName("Verify healthy building the vanilla regex")
	@Test
	public void test1() {
		assertDoesNotThrow(() -> {
			for (String copyBook : rows) {
				Pattern pattern = Pattern.compile(regexString);
				Matcher matcher = pattern.matcher(copyBook);
				matcher.find();
			}
		});

	}

	@DisplayName("Verify regex matches all simplified copyBook samples")
	@Test
	public void test2() {
		assertDoesNotThrow(() -> {
			Pattern pattern = Pattern.compile(regexString);

			for (String copyBook : rows) {
				Matcher matcher = pattern.matcher(copyBook);
				System.err.println(String.format("matching \"%s\"", copyBook));
				System.err.println(String.format("regex: %s", regexString));
				assertThat(matcher.matches(), is(true));
				// @formatter:off
				System.out.println(String.format(
						"Matched record:%n" + "  BRANCH   = %s%n" + "  TRANDATE = %s%n" + "  ACCOUNT  = %s%n"
								+ "  CODE     = %s%n" + "  AMOUNT   = %s%n" + "  CURRENCY = %s",
						matcher.group("BRANCH"), matcher.group("TRANDATE"), matcher.group("ACCOUNT"),
						matcher.group("CODE"), matcher.group("AMOUNT"), matcher.group("CURRENCY")));
				// @formatter:on

			}
		});
	}

	@DisplayName("Verify the resolve groups processing")
	@Test
	public void test3() {
		List<String> groups = findMatch.resolveGroups(regexString);
		assertThat(groups, notNullValue());
		assertThat(groups.size(), is(6));
		groups.stream().forEach(System.err::println);
	}

	@DisplayName("Verify processing by FindMatch")
	@Test
	public void test4() {

		for (String copyBook : rows) {
			System.err.println(String.format("copyBook: %s", copyBook));
			results = findMatch.findMatch(copyBook, regexString);
			assertThat(results, notNullValue());
			assertThat(results.keySet().size(), is(6));
			for (String name : results.keySet()) {
				String result = results.get(name);
				System.err.println(String.format("%s: %s", name, result));
			}
		}
	}

	@DisplayName("Verify the results serialization")
	@Test
	public void test5() {

		for (String copyBook : rows) {
			System.err.println(String.format("copyBook: %s", copyBook));
			results = findMatch.findMatch(copyBook, regexString);
			assertThat(results, notNullValue());

			Gson gson = new GsonBuilder()
					.registerTypeAdapter(CopyBook.class, new CopyBookGsonSerializer(Set.of("BRANCH"))).create();
			String json = gson.toJson(new CopyBook(results));
			System.err.println(String.format("JSON: %s", json));
		}
	}

	@SuppressWarnings("unchecked")
	@DisplayName("Verify the results filtering")
	@Test
	public void test6()
			throws IllegalArgumentException, IllegalAccessException, NoSuchFieldException, SecurityException {

		// Set<String> excluded = Set.of("BRANCH"); // effecively final
		Set<String> excluded =  new HashSet<>(Set.of("BRANCH"));
		CopyBookGsonSerializer copyBookGsonSerializer = new CopyBookGsonSerializer();
		// Class<?> copyBookGsonSerializerClass = copyBookGsonSerializer.getClass();
		Field hardCodedExcludedKeys = copyBookGsonSerializer.getClass().getDeclaredField("hardCodedExcludedKeys");
		hardCodedExcludedKeys.setAccessible(true);

		excluded.addAll((Set<String>) hardCodedExcludedKeys.get(copyBookGsonSerializer));
		Map<String, String> results = Map.of("BRANCH", "DEP", "CURRENCY", "USD", "ACCOUNT", "1234000001", "CODE", "DEP",
				"AMOUNT", "000000789", "TRANDATE", "20230615");

		CopyBook copybook = new CopyBook(results);

		Gson gson = new GsonBuilder().registerTypeAdapter(CopyBook.class, new CopyBookGsonSerializer(excluded))
				.create();

		String json = gson.toJson(copybook);
		for (Map.Entry<String, String> entry : results.entrySet()) {
			String key = entry.getKey();
			String expectedValue = entry.getValue();

			if (excluded.contains(key)) {
				// excluded field should not be serialized — JUnit assertThrows
				assertThrows(com.jayway.jsonpath.PathNotFoundException.class, () -> JsonPath.read(json, "$." + key));
			} else {
				// Other fields should exist and match expected values
				assertThat(JsonPath.read(json, "$." + key), equalTo(expectedValue));
			}
			// EIBCALEN
			assertThrows(PathNotFoundException.class, () -> JsonPath.read(json, "$.EIBCALEN"));
		}
	}
}
