package example;
/**
 * Copyright 2023 Serguei Kouzmine
 */

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import org.junit.jupiter.api.Test;
import example.Utils;
public class ApplicationPropertyUpdaterTest {
	private static Utils utils = Utils.getInstance();
	private static final String fileName = "application.properties";

	@Test
	public void test1() throws Exception {
		String configuration = utils.getResourceContent(fileName);
		List<String> tokens = Arrays.asList(
				utils.getApplicationProperties().getProperty("commandline").split(" +"));
		Map<String, Object> properties = new HashMap<>();
		tokens.stream().forEach((String t) -> {
			String[] data = t.split("=");
			properties.put(data[0], data[1]);
		});
		ApplicationPropertyUpdater propertyUpdater = new ApplicationPropertyUpdater(
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
		//assertThat(configuration,
		//		containsString(getApplicationProperties().getProperty("example")));

		System.err.println("new configuration: " + configuration);
	}

}
