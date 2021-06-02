package example;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import org.apache.commons.configuration.ConfigurationException;
import org.testng.annotations.Test;

public class PropertyTest {
	private final String key = "browser";
	private final String value = System.getProperty(key, "chrome");

	@Test
	public void test1() {
		assertThat(PropertiesParser
				.getProperties("application.properties", System.getProperty("user.dir") + "/target/classes/", false)
				.get(key), is(value));

	}

	@Test
	public void test2() {
		try {
			assertThat(PropertiesParser.readProperty(key, "../classes/" + "application.properties"), is(value));
		} catch (ConfigurationException e) {
		}
	}

	@Test
	public void test3() {
		try {
			assertThat(PropertiesParser.readProperty(key), is("${browser.setting}"));
		} catch (ConfigurationException e) {
		}
	}
}
