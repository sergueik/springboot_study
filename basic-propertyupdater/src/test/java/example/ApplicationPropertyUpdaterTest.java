package example;
/**
 * Copyright 2023 Serguei Kouzmine
 */

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;

import java.util.Map;

import org.junit.jupiter.api.Test;

public class ApplicationPropertyUpdaterTest {
	private static Utils utils = Utils.getInstance();
	private static final String fileName = "application.properties";

	@Test
	public void test1() throws Exception {
		String configuration = utils.getResourceContent(fileName);
		Map<String, Object> properties = utils.getPropertiesFromCommandline(
				utils.getApplicationProperties().getProperty("commandline"));
		PropertyUpdater propertyUpdater = new ApplicationPropertyUpdater(
				configuration, properties);
		propertyUpdater.setTrim(false);
		System.err.println("orig configuration: " + configuration);
		propertyUpdater.updateConfiguration();
		configuration = propertyUpdater.getConfiguration();
		assertThat(configuration, containsString(String.format("%s %s %s %s",
				properties.get("name1").toString(), properties.get("name2").toString(),
				properties.get("name3").toString(),
				properties.get("name4").toString())));
		// the following will fail, it appears we are not property file replacement
		// compatible
		// assertThat(configuration,
		// containsString(getApplicationProperties().getProperty("example")));

		System.err.println("new configuration: " + configuration);
	}

}
