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

import org.junit.jupiter.api.Test;

public class UdeployPropertyUpdaterTest {
	private static Utils utils = Utils.getInstance();
	private static final String fileName = "application.yaml";
	private static final String expectedFileName = "configured_application.yaml";
	private static final String projectName = "springboot_study/basic-propertyupdater";

	@Test
	public void test1() throws Exception {
		String configuration = utils.getResourceContent(fileName);
		Map<String, Object> properties = utils.getPropertiesFromCommandline(
				utils.getApplicationProperties().getProperty("commandline"));

		UdeployPropertyUpdater propertyUpdater = new UdeployPropertyUpdater(
				configuration, properties);
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

	@Test
	public void test2() throws Exception {
		String filepath = Paths
				.get(System.getProperty("user.dir") + "/" + String
						.format("../../%s/src/test/resources/%s", projectName, fileName))
				.normalize().toAbsolutePath().toString();
		String configuration = utils.getFileContent(filepath);
		System.err.println("Original configuration: " + configuration);
	}

	@Test
	public void test3() throws Exception {
		String filepath = Paths
				.get(System.getProperty("user.dir") + "/" + String.format(
						"../../%s/src/test/resources/%s%s", projectName, fileName, ".tmp"))
				.normalize().toAbsolutePath().toString();
		List<String> content = Arrays
				.asList(new String[] { "this", "is", "a", "test" });
		utils.writeToFile(content, filepath, true);
		String configuration = utils.getFileContent(filepath);
		System.err.println("New configuration: " + configuration);
	}

}
