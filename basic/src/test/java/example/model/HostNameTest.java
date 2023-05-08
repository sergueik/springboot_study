package example.model;

import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.is;

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

}
