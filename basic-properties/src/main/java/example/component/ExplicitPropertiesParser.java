package example.component;

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

import org.springframework.stereotype.Component;

/**
 * Common configuration / properties file parser
 * 
 * @author: Serguei Kouzmine (kouzmine_serguei@yahoo.com)
 */

@Component
public class ExplicitPropertiesParser {

	// will not be able to make a static reference to the non-static field
	// properties, so the following is useless:

	// private Properties properties;
	// public SecondComponent(@Autowired Properties properties) {
	// this.properties = properties;
	// }

	private final static boolean debug = false;
	private static final String propertiesFileName = "application.properties";
	private static final boolean fromThread = false;
	private static final String propertiesFilePath = "";
	private static Map<String, String> properties = getProperties(propertiesFileName, propertiesFilePath, fromThread);
	protected static String osName = getOSName();

	public static String getSomeProperty() {
		if (getOSName().equals("windows"))
			return properties.get("example.firstProperty");
		else
			return properties.get("example.secondProperty");
	}

	// possibly use one of shorthand methods
	private static Map<String, String> getProperties(final String propertiesFileName) {
		return getProperties(propertiesFileName, null, false);
	}

	public static Map<String, String> getProperties(final String propertiesFileName, boolean fromThread) {
		return getProperties(propertiesFileName, null, fromThread);
	}

	public static Map<String, String> getProperties(final String propertiesFileName, String propertiesFilePath) {
		return getProperties(propertiesFileName, propertiesFilePath, false);
	}

	public static Map<String, String> getProperties(final String propertiesFileName, String propertiesFilePath,
			boolean fromThread) {
		final InputStream stream;
		String resourcePath = null;
		final Properties properties = new Properties();
		Map<String, String> propertiesMap = new HashMap<>();

		try {
			// only works when jar has been packaged?
			if (propertiesFilePath == null || propertiesFilePath.isEmpty()) {
				if (debug)
					System.err
							.println(String.format("Reading properties file \"%s\" from the jar", propertiesFileName));
				stream = ExplicitPropertiesParser.class.getClassLoader().getResourceAsStream(propertiesFileName);
			} else if (fromThread) {
				try {
					resourcePath = Thread.currentThread().getContextClassLoader().getResource("").getPath();
				} catch (NullPointerException e) {
					System.out.println(e.getMessage());
				}
				if (debug)
					System.err.println(String.format("Reading properties file \"%s\" from the thread context",
							resourcePath + propertiesFileName));
				stream = new FileInputStream(resourcePath + propertiesFileName);
			} else {
				propertiesFilePath = String.format("%s/%s", propertiesFilePath, propertiesFileName);
				if (debug)
					System.err.println(String.format("Reading properties file from disk: '%s'", propertiesFilePath));
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
			System.err.println(String.format("Properties file was not found or not readable: \"%s\". %s",
					propertiesFileName, e.toString()));
		}
		return (propertiesMap);
	}

	public static String getOSName() {
		if (osName == null) {
			osName = System.getProperty("os.name").toLowerCase();
			if (osName.startsWith("windows")) {
				osName = "windows";
			}
		}
		return osName;
	}

}
