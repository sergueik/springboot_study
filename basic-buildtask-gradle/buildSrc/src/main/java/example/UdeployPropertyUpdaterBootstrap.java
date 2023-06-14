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
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import javax.inject.Inject;

import example.UdeployPropertyUpdater;
import example.Utils;

class UdeployPropertyUpdaterBootstrap {

	private String fileName = "application.yaml";
	private String filepath = "buildSrc/src/main/resources";
	private String commandline = null;
	private boolean save = true;
	private static Utils utils = Utils.getInstance();

	public void setFileName(String value) {
		fileName = value;
	}

	public void setCommandline(String value) {
		commandline = value;
	}

	public UdeployPropertyUpdaterBootstrap(String fileName) {

		super();
		this.fileName = fileName;
	}

	public UdeployPropertyUpdaterBootstrap(String fileName, String commandline) {

		super();
		this.commandline = commandline;
		this.fileName = fileName;
	}

	public void process() throws IOException {
		if (commandline == null)
			commandline = utils.getApplicationProperties().getProperty("commandline");

		String configuration = utils.getResourceContent(fileName);

		String configurationFilePath = Paths
				.get(String.format("%s/%s/%s", System.getProperty("user.dir"), filepath, fileName)).normalize()
				.toAbsolutePath().toString();
		configuration = utils.getFileContent(configurationFilePath);

		System.out.println("template configuration: " + configuration);
		List<String> tokens = Arrays.asList(commandline.split(" +"));
		Map<String, Object> properties = new HashMap<>();
		tokens.stream().forEach((String t) -> {
			String[] data = t.split("=");
			properties.put(data[0], data[1]);
		});
		UdeployPropertyUpdater propertyUpdater = new UdeployPropertyUpdater(configuration, properties);
		propertyUpdater.updateConfiguration();
		configuration = propertyUpdater.getConfiguration();
		System.err.println("new configuration: " + configuration);
		if (save)
			utils.writeToFile(Arrays.asList(configuration.split("\n")), configurationFilePath, true);

	}

}
