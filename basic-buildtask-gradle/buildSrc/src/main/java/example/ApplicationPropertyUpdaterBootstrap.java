package example;
/**
 * Copyright 2023 Serguei Kouzmine
 */

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

import javax.inject.Inject;

import example.ApplicationPropertyUpdater;

class ApplicationPropertyUpdaterBootstrap {

	private String fileName = "gradle.properties";
	private String commandline = null;

	public void setFileName(String value) {
		fileName = value;
	}

	public void setCommandline(String value) {
		commandline = value;
	}

	public ApplicationPropertyUpdaterBootstrap(String fileName) {

		super();
		this.fileName = fileName;
	}

	public ApplicationPropertyUpdaterBootstrap(String fileName,
			String commandline) {

		super();
		this.commandline = commandline;
		this.fileName = fileName;
	}

	public void process() throws IOException {
		if (commandline == null)
			commandline = getApplicationProperties().getProperty("commandline");
		
		String configuration = getResourceContent(fileName);
		System.out.println("orig configuration: " + configuration);
		List<String> tokens = Arrays.asList(commandline.split(" +"));
		Map<String, Object> properties = new HashMap<>();
		tokens.stream().forEach((String t) -> {
			String[] data = t.split("=");
			properties.put(data[0], data[1]);
		});
		ApplicationPropertyUpdater propertyUpdater = new ApplicationPropertyUpdater(
				configuration, properties);
		propertyUpdater.setTrim(false);
		propertyUpdater.updateConfiguration();
		configuration = propertyUpdater.getConfiguration();
		System.err.println("new configuration: " + configuration);
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

}
