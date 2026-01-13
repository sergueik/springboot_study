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

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;

import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import example.FindMatch;

public class RegexScaleDemoTest {
	
private static final String copyBook = "BR001T12345202401301030151234567890120000000342391USDBR001ATM WITHDRAWAL                APR123BR001T12345202401301030151234567890120000000342391USDBR001ATM WITHDRAWAL                APR123BR001T12345202401301030151234567890120000000342391USDBR001ATM WITHDRAWAL                APR123BR001T12345202401301030151234567890120000000342391USDBR001ATM WITHDRAWAL                APR123BR001T12345202401301030151234567890120000000342391USDBR001ATM WITHDRAWAL                APR123BR001T12345202401301030151234567890120000000342391USDBR001ATM WITHDRAWAL                APR123BR001T12345202401301030151234567890120000000342391USDBR001ATM WITHDRAWAL                APR123BR001T12345202401301030151234567890120000000342391USDBR001ATM WITHDRAWAL                APR123BR001T12345202401301030151234567890120000000342391USDBR001ATM WITHDRAWAL                APR123BR001T12345202401301030151234567890120000000342391USDBR001ATM WITHDRAWAL                APR123";
private FindMatch findMatch = new FindMatch();
private final static boolean directConversion = true;
// plain 1/1 conversion
// https://futurestud.io/tutorials/gson-getting-started-with-java-json-serialization-deserializationhttps://futurestud.io/tutorials/gson-getting-started-with-java-json-serialization-deserializationhttps://futurestud.io/tutorials/gson-getting-started-with-java-json-serialization-deserialization
Gson gson = directConversion ? new Gson()
		: new GsonBuilder()
				.registerTypeAdapter(Map.class, new CopyBookGsonSerializer())
				.create();
public static final String regexString = "^" +
"(?<BRANCHID1>.{5})" +
"(?<TELLERID1>.{6})" +
"(?<TRANDATE1>\\d{8})" +
"(?<TRANTIME1>\\d{6})" +
"(?<ACCOUNTNUMBER1>\\d{12})" +
"(?<AMOUNT1>[+-]?\\d{13})" +
"(?<CURRENCY1>[A-Z]{3})" +
"(?<BRANCH1>.{5})" +
"(?<DESCRIPTION1>.{30})" +
"(?<APPROVALCODE1>.{6})" +
"(?<BRANCHID2>.{5})" +
"(?<TELLERID2>.{6})" +
"(?<TRANDATE2>\\d{8})" +
"(?<TRANTIME2>\\d{6})" +
"(?<ACCOUNTNUMBER2>\\d{12})" +
"(?<AMOUNT2>[+-]?\\d{13})" +
"(?<CURRENCY2>[A-Z]{3})" +
"(?<BRANCH2>.{5})" +
"(?<DESCRIPTION2>.{30})" +
"(?<APPROVALCODE2>.{6})" +
"(?<BRANCHID3>.{5})" +
"(?<TELLERID3>.{6})" +
"(?<TRANDATE3>\\d{8})" +
"(?<TRANTIME3>\\d{6})" +
"(?<ACCOUNTNUMBER3>\\d{12})" +
"(?<AMOUNT3>[+-]?\\d{13})" +
"(?<CURRENCY3>[A-Z]{3})" +
"(?<BRANCH3>.{5})" +
"(?<DESCRIPTION3>.{30})" +
"(?<APPROVALCODE3>.{6})" +
"(?<BRANCHID4>.{5})" +
"(?<TELLERID4>.{6})" +
"(?<TRANDATE4>\\d{8})" +
"(?<TRANTIME4>\\d{6})" +
"(?<ACCOUNTNUMBER4>\\d{12})" +
"(?<AMOUNT4>[+-]?\\d{13})" +
"(?<CURRENCY4>[A-Z]{3})" +
"(?<BRANCH4>.{5})" +
"(?<DESCRIPTION4>.{30})" +
"(?<APPROVALCODE4>.{6})" +
"(?<BRANCHID5>.{5})" +
"(?<TELLERID5>.{6})" +
"(?<TRANDATE5>\\d{8})" +
"(?<TRANTIME5>\\d{6})" +
"(?<ACCOUNTNUMBER5>\\d{12})" +
"(?<AMOUNT5>[+-]?\\d{13})" +
"(?<CURRENCY5>[A-Z]{3})" +
"(?<BRANCH5>.{5})" +
"(?<DESCRIPTION5>.{30})" +
"(?<APPROVALCODE5>.{6})" +
"(?<BRANCHID6>.{5})" +
"(?<TELLERID6>.{6})" +
"(?<TRANDATE6>\\d{8})" +
"(?<TRANTIME6>\\d{6})" +
"(?<ACCOUNTNUMBER6>\\d{12})" +
"(?<AMOUNT6>[+-]?\\d{13})" +
"(?<CURRENCY6>[A-Z]{3})" +
"(?<BRANCH6>.{5})" +
"(?<DESCRIPTION6>.{30})" +
"(?<APPROVALCODE6>.{6})" +
"(?<BRANCHID7>.{5})" +
"(?<TELLERID7>.{6})" +
"(?<TRANDATE7>\\d{8})" +
"(?<TRANTIME7>\\d{6})" +
"(?<ACCOUNTNUMBER7>\\d{12})" +
"(?<AMOUNT7>[+-]?\\d{13})" +
"(?<CURRENCY7>[A-Z]{3})" +
"(?<BRANCH7>.{5})" +
"(?<DESCRIPTION7>.{30})" +
"(?<APPROVALCODE7>.{6})" +
"(?<BRANCHID8>.{5})" +
"(?<TELLERID8>.{6})" +
"(?<TRANDATE8>\\d{8})" +
"(?<TRANTIME8>\\d{6})" +
"(?<ACCOUNTNUMBER8>\\d{12})" +
"(?<AMOUNT8>[+-]?\\d{13})" +
"(?<CURRENCY8>[A-Z]{3})" +
"(?<BRANCH8>.{5})" +
"(?<DESCRIPTION8>.{30})" +
"(?<APPROVALCODE8>.{6})" +
"(?<BRANCHID9>.{5})" +
"(?<TELLERID9>.{6})" +
"(?<TRANDATE9>\\d{8})" +
"(?<TRANTIME9>\\d{6})" +
"(?<ACCOUNTNUMBER9>\\d{12})" +
"(?<AMOUNT9>[+-]?\\d{13})" +
"(?<CURRENCY9>[A-Z]{3})" +
"(?<BRANCH9>.{5})" +
"(?<DESCRIPTION9>.{30})" +
"(?<APPROVALCODE9>.{6})" +
"(?<BRANCHID10>.{5})" +
"(?<TELLERID10>.{6})" +
"(?<TRANDATE10>\\d{8})" +
"(?<TRANTIME10>\\d{6})" +
"(?<ACCOUNTNUMBER10>\\d{12})" +
"(?<AMOUNT10>[+-]?\\d{13})" +
"(?<CURRENCY10>[A-Z]{3})" +
"(?<BRANCH10>.{5})" +
"(?<DESCRIPTION10>.{30})" +
"(?<APPROVALCODE10>.{6})" +
"$";

	@DisplayName("Verify if pattern constructor hits a hard JVM regex limit when building of the regex with 100 captured fields is typically 800–2000 bytes, which is normal on mainframes")
	@Test
	void test1() {
		try {
			Pattern pattern = Pattern.compile(regexString);
			Matcher matcher = pattern.matcher(copyBook);
			matcher.find();
			assertThat(matcher.matches(), is(true));
	
		} catch (PatternSyntaxException | IllegalStateException e) {
			// Report more information
			System.err.println("PatternSyntaxException! " + e.toString());
		}
	}

	@DisplayName("Verify if pattern constructor hits a hard JVM regex limit when building of the regex with 100 captured fields is typically 800–2000 bytes, which is normal on mainframes")
	@Test
	void test2() {

		try {
			Map<String, String> results = findMatch.findMatch(copyBook, regexString);
			assertThat(results, notNullValue());
			assertThat(results.keySet().size(), is(100));
			for (String name : results.keySet()) {
				String result = results.get(name);
				System.err.println(String.format("%s: %s", name, result));
			}
			System.err
			.println("JSON serialization with gson:\n" + gson.toJson(results));

		} catch (PatternSyntaxException | IllegalStateException e) {
			// Report more information
			System.err.println("PatternSyntaxException! " + e.toString());
		}
	
	}

}
