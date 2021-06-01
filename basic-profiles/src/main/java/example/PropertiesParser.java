package example;
/**
 * Copyright 2021 Serguei Kouzmine
 */

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import org.apache.commons.configuration.Configuration;
import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.PropertiesConfiguration;

/**
 * Common configuration / properties file parser
 * @author: Serguei Kouzmine (kouzmine_serguei@yahoo.com)
 */

public class PropertiesParser {

	private final static boolean debug = false;

	public static Map<String, String> getProperties(
			final String propertiesFileName) {
		return getProperties(propertiesFileName, null, false);
	}

	public static Map<String, String> getProperties() {
		return getProperties("application.properties", null, false);
	}

	public static Map<String, String> getProperties(
			final String propertiesFileName, boolean fromThread) {
		return getProperties(propertiesFileName, null, fromThread);
	}

	public static Map<String, String> getProperties(
			final String propertiesFileName, String propertiesFilePath) {
		return getProperties(propertiesFileName, propertiesFilePath, false);
	}

	public static Map<String, String> getProperties(
			final String propertiesFileName, String propertiesFilePath,
			boolean fromThread) {
		final InputStream stream;
		String resourcePath = null;
		final Properties properties = new Properties();
		Map<String, String> propertiesMap = new HashMap<>();

		try {
			// only works when jar has been packaged?
			if (propertiesFilePath == null || propertiesFilePath.isEmpty()) {
				if (debug)
					System.err.println(
							String.format("Reading properties file \"%s\" from the jar",
									propertiesFileName));
				stream = PropertiesParser.class.getClassLoader()
						.getResourceAsStream(propertiesFileName);
			} else if (fromThread) {
				try {
					resourcePath = Thread.currentThread().getContextClassLoader()
							.getResource("").getPath();
				} catch (NullPointerException e) {
					System.out.println(e.getMessage());
				}
				if (debug)
					System.err.println(String.format(
							"Reading properties file \"%s\" from the thread context",
							resourcePath + propertiesFileName));
				stream = new FileInputStream(resourcePath + propertiesFileName);
			} else {
				propertiesFilePath = String.format("%s/%s", propertiesFilePath,
						propertiesFileName);
				if (debug)
					System.err.println(String.format(
							"Reading properties file from disk: '%s'", propertiesFilePath));
				stream = new FileInputStream(propertiesFilePath);
			}
			properties.load(stream);
			@SuppressWarnings("unchecked")
			Enumeration<String> e = (Enumeration<String>) properties.propertyNames();
			for (; e.hasMoreElements();) {
				String key = e.nextElement();
				String val = properties.get(key).toString();
				if (debug)
					System.err.println(String.format("Added: \"%s\" = \"%s\"", key, val));
				propertiesMap.put(key, val);
			}
		} catch (IOException e) {
			System.err.println(String.format(
					"Properties file was not found or not readable: \"%s\". %s",
					propertiesFileName, e.toString()));
		}
		return (propertiesMap);
	}

	// based on:
	// https://github.com/abhishek8908/selenium-drivers-download-plugin/blob/master/src/main/java/com/github/abhishek8908/util/DriverUtil.java
	public static String readProperty(final String propertyName)
			throws ConfigurationException {
		return readProperty(propertyName, "application.properties");
	}

	public static String readProperty(final String propertyName,
			final String propertyFile) throws ConfigurationException {
		String resourcePath = "";
		try {
			resourcePath = Thread.currentThread().getContextClassLoader()
					.getResource("").getPath();
		} catch (NullPointerException e) {
			System.err.println(resourcePath + e.getMessage());
		}
		System.err.println("Reading: " + resourcePath + propertyFile);
		Configuration config = new PropertiesConfiguration(
				resourcePath + propertyFile);
		Configuration extConfig = ((PropertiesConfiguration) config)
				.interpolatedConfiguration();
		return extConfig.getProperty(propertyName).toString();
	}
}
