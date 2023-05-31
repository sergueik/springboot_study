package example;
/**
 * Copyright 2023 Serguei Kouzmine
 */

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import static org.hamcrest.Matchers.*;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.hasKey;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.core.StringContains.containsString;
import static org.junit.Assert.*;

import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.io.IOException;
import java.io.InputStream;

public class PropertyUpdaterTest {

	private static final String fileName = "application.yaml";
	private static final String expectedFileName = "configured_application.yaml";

	@Test
	public void test1() throws Exception {
		String configuration = getFileContent(fileName);

		List<String> tokens = Arrays.asList(
				getApplicationProperties().getProperty("commandline").split(" +"));
		Map<String, Object> properties = new HashMap<>();
		tokens.stream().forEach((String t) -> {
			String[] data = t.split("=");
			properties.put(data[0], data[1]);
		});
		PropertyUpdater propertyUpdater = new PropertyUpdater(configuration,
				properties);
		propertyUpdater.updateConfiguration();
		configuration = propertyUpdater.getConfiguration();
		System.err.println("new configuration: " + configuration);

		String expectedConfiguration = getFileContent(expectedFileName);
		String[] checkResults = expectedConfiguration.split("\r?\n");
		Set<String> result = new HashSet<String>();
		String[] lines = configuration.split("\r?\n");
		for (int cnt = 0; cnt != lines.length; cnt++) {
			result.add(lines[cnt]);
		}

		System.err.println("test returned: " + result);
		System.err.println("test expected: " + Arrays.asList(checkResults));
		assertThat(result, containsInAnyOrder(checkResults));
	}

	public String getFileContent(String fileName) {
		try {
			final InputStream stream = this.getClass().getClassLoader()
					.getResourceAsStream(fileName);
			final byte[] bytes = new byte[stream.available()];
			stream.read(bytes);
			return new String(bytes, "UTF-8");
		} catch (IOException e) {
			throw new RuntimeException(fileName);
		}
	}

	// based on:
	// http://www.java2s.com/example/java/java.util/get-application-properties.html
	public Properties getApplicationProperties() throws IOException {
		Properties appProperties = new Properties();
		InputStream in = null;
		try {
			in = this.getClass().getClassLoader()
					.getResourceAsStream("application.properties");
			appProperties.load(in);
			return appProperties;
		} finally {
			if (in != null) {
				in.close();
			}
		}
	}

}
