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
import java.util.Map;
import java.util.stream.Collectors;

public class HostNameTest {
	private static Set<String> result;

	private static final List<String> inputs = Arrays
			.asList(new String[] { "aaa-bbbbb", "cccddddd", "aa-bbb", "aa-ccc", });
	private static String[] checkResults = new String[] { "aaa", "ccc", "aa" };

	// https://www.baeldung.com/junit-assert-exception
	// https://howtodoinjava.com/junit5/expected-exception-example/
	// this is Junit 5
	@Test
	public void test2() {
		List<String> inputs2 = new ArrayList<>();
		inputs2.addAll(inputs);
		inputs2.add("zzzzzzzz");
		result = inputs2.stream().map((String o) -> {
			return new HostName(o);
		}).map((HostName o) -> o.getHostPrefix()).collect(Collectors.toSet());

		// incompatible types: inference variable T has incompatible bounds
		// equality constraints: java.lang.AssertionError

		Exception exception = assertThrows(Exception.class, () -> {
			try {
				assertThat(result, containsInAnyOrder(checkResults));
				System.err.println("Returned: " + result);
				System.err.println("Expected: " + Arrays.asList(checkResults));
			} catch (AssertionError e) {
				throw new Exception(e.getMessage());
			}
		});

		assertThat(exception.getMessage(), containsString("no match for: \"zzz\""));
	}

	@Test
	public void test3() {

		List<String> inputs2 = new ArrayList<>();
		inputs2.addAll(inputs);
		inputs2.remove("cccddddd");
		result = inputs2.stream().map((String o) -> {
			return new HostName(o);
		}).map((HostName o) -> o.getHostPrefix()).collect(Collectors.toSet());

		Exception exception = assertThrows(Exception.class, () -> {
			try {
				assertThat(result, containsInAnyOrder(checkResults));
			} catch (AssertionError e) {
				throw new Exception(e.getMessage());
			}
			System.err.println("Returned: " + result);
			System.err.println("Expected: " + Arrays.asList(checkResults));

		});

		assertThat(exception.getMessage(),
				containsString("no item matches: \"ccc\""));
	}

	@Test
	public void test4() {
		// inputs.remove("aaa-bbbbb");
		// java.lang.UnsupportedOperationException
		Exception exception = assertThrows(UnsupportedOperationException.class,
				() -> {
					// but not here
					inputs.remove("aaa-bbbbb");
				});

		assertThat(exception.getMessage(), nullValue());
	}
}
