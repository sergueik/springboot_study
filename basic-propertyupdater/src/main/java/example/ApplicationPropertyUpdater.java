package example;
/**
 * Copyright 2023 Serguei Kouzmine
 */

import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class ApplicationPropertyUpdater implements PropertyUpdater {
	private String configuration = null;
	private Map<String, Object> properties;
	// private static String osName = OSUtils.getOsName();
	private boolean debug = false;
	private boolean trim = false;

	public String getConfiguration() {
		return configuration;
	}

	public void setConfiguration(String value) {
		configuration = value;
	}

	public void setTrim(boolean value) {
		trim = value;
	}

	public void setProperties(Map<String, Object> value) {
		properties = value;
	}

	public ApplicationPropertyUpdater(String configuration,
			Map<String, Object> properties) {
		this.configuration = configuration;
		this.properties = properties;
	}

	public void updateConfiguration() {
		configuration = resolveEnvVars(configuration);
	}

	private String getPropertyEnv(String name, String defaultValue) {
		String value = System.getProperty(name);
		if (debug) {
			System.err.println("Getting propety or environment: " + name);
		}
		// compatible with
		// org.apache.commons.configuration.PropertiesConfiguration.interpolatedConfiguration
		// https://commons.apache.org/proper/commons-configuration/userguide_v1.10/howto_utilities.html
		if (value == null) {

			Pattern p = Pattern.compile("^(\\w+:)(\\w+)$");
			Matcher m = p.matcher(name);
			if (m.find()) {
				String n = m.replaceFirst("$2");
				if (debug) {
					System.err.println("Interpolating " + n);
				}
				value = System.getProperty(n);
			}
			if (value == null) {
				if (debug) {
					System.err.println("Trying environment " + name);
				}
				value = System.getenv(name);
				if (value == null) {
					if (debug) {
						System.err.println("Nothing found for " + name);
					}
					value = defaultValue;
				}
			}
		}
		return value;
	}

	private String resolveEnvVars(String input) {
		if (null == input) {
			return null;
		}
		Pattern p = Pattern.compile("\\$(?:\\{(\\w+)\\}|(\\w+))");
		Matcher m = p.matcher(input);
		StringBuffer sb = new StringBuffer();
		while (m.find()) {
			String n = null == m.group(1) ? m.group(2) : m.group(1);
			String v = properties.containsKey(n) ? properties.get(n).toString()
					: System.getenv(n);
			if (trim) {
				m.appendReplacement(sb,
						null == v || v.replaceAll("\"", "").trim().length() == 0 ? ""
								: v.replace("\\", "\\\\"));
			} else {
				m.appendReplacement(sb,
						null == v || v.trim().length() == 0 ? "" : v.replace("\\", "\\\\"));

			}
		}
		m.appendTail(sb);
		return sb.toString();
	}

}
