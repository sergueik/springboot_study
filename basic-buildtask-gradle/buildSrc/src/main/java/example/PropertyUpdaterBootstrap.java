package example;

/**
 * Copyright 2023 Serguei Kouzmine
 */
import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import example.PropertyUpdater;

import javax.inject.Inject;

class PropertyUpdaterBootstrap {
	private String tool = "perl";

	public void setTool(String value) {
		tool = value;
	}

	private String fileName = "application.yaml";

	public void setFileName(String value) {
		fileName = value;
	}

	private String commandline = null;

	public void setCommandline(String value) {
		commandline = value;
	}
	public PropertyUpdaterBootstrap(String tool, String fileName) {

		super();
		this.tool = tool;
		this.fileName = fileName;
	}

	public PropertyUpdaterBootstrap(String tool, String fileName, String commandline) {

		super();
		this.tool = tool;
		this.commandline = commandline;
		this.fileName = fileName;
	}
	public void process() throws IOException {
	if (commandline == null)	
		commandline = getApplicationProperties().getProperty("commandline");
		System.out.println("Processing tool: " + tool);
		String configuration = getFileContent(fileName);
		System.out.println("new configuration: " + configuration);
		List<String> tokens = Arrays.asList( commandline.split(" +"));
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

		switch (tool) {
		case "java":
			System.out.println(System.getProperty("java.version"));
			break;
		case "groovy":
			System.out.println("TODO: GroovySystem.getVersion()");
			break;
		default:
			throw new IllegalArgumentException("Unknown tool: " + tool);
		}
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
