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
import example.Utils;

class ApplicationPropertyUpdaterBootstrap {

	private String fileName = "gradle.properties";
	private String commandline = null;
	private static Utils utils = Utils.getInstance();

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
			commandline = utils.getApplicationProperties().getProperty("commandline");
		
		String configuration = utils.getResourceContent(fileName);
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

}
