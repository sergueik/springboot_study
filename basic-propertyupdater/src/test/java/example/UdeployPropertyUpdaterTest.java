package example;
/**
 * Copyright 2023 Serguei Kouzmine
 */

import org.junit.jupiter.api.Test;

import static org.hamcrest.Matchers.*;
import static org.hamcrest.MatcherAssert.assertThat;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.file.Paths;

import example.UdeployPropertyUpdater;

public class UdeployPropertyUpdaterTest {

	private static final String fileName = "application.yaml";
	private static final String expectedFileName = "configured_application.yaml";
	private static final String projectName = "springboot_study/basic-propertyupdater";

	@Test
	public void test1() throws Exception {
		String configuration = getResourceContent(fileName);
		List<String> tokens = Arrays.asList(
				getApplicationProperties().getProperty("commandline").split(" +"));
		Map<String, Object> properties = new HashMap<>();
		tokens.stream().forEach((String t) -> {
			String[] data = t.split("=");
			properties.put(data[0], data[1]);
		});
		UdeployPropertyUpdater propertyUpdater = new UdeployPropertyUpdater(configuration,
				properties);
		propertyUpdater.updateConfiguration();
		configuration = propertyUpdater.getConfiguration();
		System.err.println("new configuration: " + configuration);

		String expectedConfiguration = getResourceContent(expectedFileName);
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
		String configuration = getFileContent(filepath);
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
		writeToFile(content, filepath, true);
		String configuration = getFileContent(filepath);
		System.err.println("New configuration: " + configuration);
	}

	public String getResourceContent(String fileName) {
		try {
			final InputStream stream = this.getClass().getClassLoader()
					.getResourceAsStream(fileName);
			final byte[] bytes = new byte[stream.available()];
			stream.read(bytes);
			return new String(bytes, "UTF-8");
		} catch (IOException e) {
			throw new RuntimeException("No resource found: " + fileName);
		}
	}

	public String getFileContent(String filePath) {
		try {
			List<String> lines = readFileLineByLine(filePath);
			return String.join("\n", lines);
		} catch (IOException e) {
			throw new RuntimeException("file not found: " + filePath);
		}
	}

	public static List<String> readFileLineByLine(String filePath)
			throws IOException {
		FileInputStream fis = new FileInputStream(filePath);
		BufferedReader br = new BufferedReader(new InputStreamReader(fis));
		List<String> res = new ArrayList<>();

		String line = null;
		while ((line = br.readLine()) != null) {
			res.add(line);
		}
		br.close();

		return res;
	}

	public static void writeToFile(List<String> content, String filePath,
			Boolean overwriteFlag) {
		File file = new File(filePath);
		if (overwriteFlag) {

			try {
				file.createNewFile();
				FileWriter fw = null;
				try {
					fw = new FileWriter(file.getAbsoluteFile());
					BufferedWriter bw = new BufferedWriter(fw);
					for (String line : content) {
						bw.write(line);
						bw.newLine();
					}
					bw.close();
				} catch (IOException e) {
					e.printStackTrace();
				}
				System.out.println("Written content to " + filePath + " succesfully!");
			} catch (IOException e) {
				e.printStackTrace();
			}
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
