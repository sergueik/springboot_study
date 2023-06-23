package example;
/**
 * Copyright 2023 Serguei Kouzmine
 */

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;

import java.nio.file.Paths;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import example.CustomPropertyUpdater;
import example.PropertyUpdater;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

public class CustomPropertyUpdaterTest {
	private static Utils utils = Utils.getInstance();
	private static final String fileName = "application.properties.custom";
	private static final String expectedFileName = "configured_application.properties.custom";
	private static final String projectName = "springboot_study/basic-propertyupdater";

	@Test
	public void test1() throws Exception {
		String configuration = utils.getResourceContent(fileName);
		Map<String, Object> properties = utils.getPropertiesFromCommandline(
				utils.getApplicationProperties().getProperty("commandline"));

		PropertyUpdater propertyUpdater = new CustomPropertyUpdater();
		propertyUpdater.setConfiguration(configuration);
		propertyUpdater.setProperties(properties);
		propertyUpdater.setTrim(true);
		propertyUpdater.updateConfiguration();
		configuration = propertyUpdater.getConfiguration();
		System.err.println("new configuration: " + configuration);

		String expectedConfiguration = utils.getResourceContent(expectedFileName);
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

	@Disabled
	@Test
	public void test2() throws Exception {
		String configuration = utils.getResourceContent(fileName);
		Map<String, Object> properties = utils.getPropertiesFromCommandline(
				utils.getApplicationProperties().getProperty("commandline"));

		PropertyUpdater propertyUpdater = new CustomPropertyUpdater();
		propertyUpdater.setConfiguration(configuration);
		propertyUpdater.setProperties(properties);
		propertyUpdater.setTrim(false);
		propertyUpdater.updateConfiguration();
		configuration = propertyUpdater.getConfiguration();
		System.err.println("new configuration: " + configuration);

		String expectedConfiguration = utils.getResourceContent(expectedFileName);
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
}
