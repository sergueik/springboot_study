package example;

import static java.lang.System.err;
import static java.lang.System.out;

import java.io.File;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;

import java.io.IOException;
import java.net.MalformedURLException;
import org.apache.commons.configuration.ConfigurationException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import java.util.concurrent.TimeUnit;
import java.util.logging.Level;

// https://www.baeldung.com/junit-before-beforeclass-beforeeach-beforeall

import org.testng.annotations.BeforeClass;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class PropertyTest {
	private final String key = "browser";
	private final String value = System.getProperty(key);

	@Test
	public void test1() {
		try {
			assertThat(PropertiesParser
					.getProperties("application.properties",
							System.getProperty("user.dir") + "/target/classes/", false)
					.get(key), is(value));

		} catch (Exception e) {
		}
	}

	@Test
	public void test2() {
		try {
			assertThat(PropertiesParser.readProperty(key,
					"../classes/" + "application.properties"), is(value));
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
