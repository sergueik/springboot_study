package example.model;

import org.junit.Before;
import org.junit.Ignore;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.is;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Set;
import java.util.Map;
import java.util.stream.Collectors;

public class HostNameTest {
	private static Set<String> result;

	private final static List<String> inputs = Arrays
			.asList(new String[] { "aaa-bbbbb", "cccddddd", "aa-bbb", "aa-ccc", });
	private static String[] checkResults = new String[] { "aaa", "ccc", "aa" };

	@Test
	public void test1() {
		result = inputs.stream().map((String o) -> {
			return new HostName(o);
		}).map((HostName o) -> o.getHostPrefix()).collect(Collectors.toSet());
		result = inputs.stream().map((String o) -> new HostName(o))
				.map((HostName o) -> o.getHostPrefix()).collect(Collectors.toSet());
		result = inputs.stream().map(HostName::new)
				.map((HostName o) -> o.getHostPrefix()).collect(Collectors.toSet());
		result = inputs.stream().map(HostName::new).map(HostName::getHostPrefix)
				.collect(Collectors.toSet());
		assertThat(result, containsInAnyOrder(checkResults));
		System.err.println("Returned: " + result);
		System.err.println("Expected: " + Arrays.asList(checkResults));
	}

	@Rule
	public ExpectedException exceptionRule = ExpectedException.none();

	@Test
	public void test3() {
		exceptionRule.expect(AssertionError.class);
		exceptionRule.expectMessage("Not matched: \"zzz\"");

		List<String> inputs2 = new ArrayList<>();
		inputs2.addAll(inputs);
		inputs2.add("zzz-zzzzz");

		result = inputs2.stream().map((String o) -> {
			return new HostName(o);
		}).map((HostName o) -> o.getHostPrefix()).collect(Collectors.toSet());
		// try {
		assertThat(result, containsInAnyOrder(checkResults));
		// System.err.println("Returned: " + result);
		// System.err.println("Expected: " + Arrays.asList(checkResults));
		//
		// } catch (Exception e) {
		// System.err.println("Exception: " + e.toString());
		// throw e;

		// }

	}

	@Test
	public void test4() {
		exceptionRule.expect(AssertionError.class);
		exceptionRule.expectMessage("No item matches: \"aaa\"");

		List<String> inputs2 = new ArrayList<>();
		inputs2.addAll(inputs);
		inputs2.remove("aaa-bbbbb");

		result = inputs2.stream().map((String o) -> {
			return new HostName(o);
		}).map((HostName o) -> o.getHostPrefix()).collect(Collectors.toSet());
		// try {
		assertThat(result, containsInAnyOrder(checkResults));
		// System.err.println("Returned: " + result);
		// System.err.println("Expected: " + Arrays.asList(checkResults));
		//
		// } catch (Exception e) {
		// System.err.println("Exception: " + e.toString());
		// throw e;

		// }

	}

	// https://www.baeldung.com/junit-assert-exception
	// this is Junit 5
	/*
	@Test
	public void test2() {
		inputs.add("zzzzzzzz");
		result = inputs.stream().map((String o) -> {
			return new HostName(o);
		}).map((HostName o) -> o.getHostPrefix()).collect(Collectors.toSet());
	
		Exception exception = assertThrows(NumberFormatException.class, () -> {
	
			assertThat(result, containsInAnyOrder(checkResults));
			System.err.println("Returned: " + result);
			System.err.println("Expected: " + Arrays.asList(checkResults));
	
		});
	
		String expectedMessage = "For input string";
		String actualMessage = exception.getMessage();
	
		assertTrue(actualMessage.contains(expectedMessage));
	}
	
	}*/
	/*
		@Test
		public void test3() {
			inputs.remove("cccddddd");
			result = inputs.stream().map((String o) -> {
				return new HostName(o);
			}).map((HostName o) -> o.getHostPrefix()).collect(Collectors.toSet());
	
			Exception exception = assertThrows(NumberFormatException.class, () -> {
	
				assertThat(result, containsInAnyOrder(checkResults));
				System.err.println("Returned: " + result);
				System.err.println("Expected: " + Arrays.asList(checkResults));
	
			});
	
			String expectedMessage = "For input string";
			String actualMessage = exception.getMessage();
	
			assertTrue(actualMessage.contains(expectedMessage));
		}
	
	}	
	*/
}
