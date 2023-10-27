package example.model;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

// https://www.lenar.io/junit5-dataprovider-analogue-example/
public class ParameterizedHostNameTest {
	// NOTE: trailing/hanging comma is OK
	public static Object[][] testData() {
		return new Object[][] { { "aa-1ibqg2e7aeiqnbcck", "aa", 4 },
				{ "aa-2fg1yh4maaprikjuo", "aa", 11 },
				{ "aaa708b4x9gnpvsxqdhz", "aaa", 0 },
				// { "aaa708b4x9gnpvsxqdhz", "bbb", 0 }
		};
	}

	@ParameterizedTest
	@MethodSource("testData")
	public void dataProviderTest(String hostname, String hostprefix, int dummy) {
		HostName obj = new HostName(hostname);
		assertThat(obj.getHostPrefix(), is(hostprefix));
	}
}

