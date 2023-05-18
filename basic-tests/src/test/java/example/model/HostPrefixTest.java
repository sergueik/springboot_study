package example.model;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.CoreMatchers.containsString;

import static org.hamcrest.Matchers.is;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertThrows;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.Map;
import java.util.stream.Collectors;

public class HostPrefixTest {

	private static Set<String> result;
	private static final String suffix = "ys4jaw6dniqc1bmq";
	private static String prefix;
	private final static Pattern pattern = Pattern.compile("^([^-.]+)[-.]?.*$");
	private static final List<String> inputs = Arrays.asList(new String[] {
			"aa-1ibqg2e7aeiqnbcck", "aa-2fg1yh4maaprikjuo", "aaa3wtw4zrn8v2dbtl6s",
			"aaa4df0iva3sebbkcnn5", "ccc5cwje3bcgz0qy1if9", "ccc6nhau7oktm2qjgmtb",
			"aaa708b4x9gnpvsxqdhz", "aaa8pdan4aftvho8yrne", "aa-9sthbd3khq4fqltob",
			"aa-0pjx31tcyuy0ubcvo" });
	private static String[] checkResults = new String[] { "aaa", "ccc", "aa" };

	private Set<String> getPrefixes(List<String> inputs) {

		return inputs.stream().map((String value) -> {
			value = pattern.matcher(value).replaceAll("$1");
			return (value.length() < 3) ? value : value.substring(0, 3);
		}).collect(Collectors.toSet());

	}

	// https://www.baeldung.com/junit-assert-exception
	// https://howtodoinjava.com/junit5/expected-exception-example/
	@Test
	public void test6() {
		List<String> inputs2 = new ArrayList<>();
		inputs2.addAll(inputs);
		prefix = "zzz";

		for (String exrahost : Arrays
				.asList(new String[] { String.format("%s%s", prefix, suffix),
						String.format("%s-%s", prefix, suffix),
						String.format("%sabc-%s", prefix, suffix) })) {
			inputs2.clear();
			inputs2.addAll(inputs);
			inputs2.add(exrahost);
			result = getPrefixes(inputs2);
			Exception exception = assertThrows(Exception.class, () -> {
				try {
					System.err.println("test6 returned: " + result);
					System.err.println("test6 expected: " + Arrays.asList(checkResults));
					assertThat(result, containsInAnyOrder(checkResults));
				} catch (AssertionError e) {
					throw new Exception(e.getMessage());
				}
			});

			assertThat(exception.getMessage(),
					containsString(String.format("no match for: \"%s\"", prefix)));
		}
	}

	@Test
	public void test5() {

		List<String> inputs2 = new ArrayList<>();
		inputs2.addAll(inputs);
		prefix = "aaa";
		List<String> inputs3 = inputs2.stream()
				.filter((String o) -> o.startsWith(prefix))
				.collect(Collectors.toList());
		inputs2.removeAll(inputs3);

		result = getPrefixes(inputs2);
		Exception exception = assertThrows(Exception.class, () -> {
			try {
				System.err.println("test5 returned: " + result);
				System.err.println("test5 expected: " + Arrays.asList(checkResults));
				assertThat(result, containsInAnyOrder(checkResults));
			} catch (AssertionError e) {
				throw new Exception(e.getMessage());
			}

		});

		assertThat(exception.getMessage(),
				containsString(String.format("no item matches: \"%s\"", prefix)));
	}

}
