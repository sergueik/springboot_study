package example.utils;

import java.io.IOException;
import java.io.InputStream;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import example.configuration.ValuesConfig;

public class Utils {

	ValuesConfig valueConfig = new ValuesConfig();

	public String getValue() {
		final InputStream stream = Utils.class.getClassLoader().getResourceAsStream("application.properties");
		Map<String, String> propertiesMap = new HashMap<>();
		final Properties properties = new Properties();
		try {
			properties.load(stream);
			@SuppressWarnings("unchecked")
			Enumeration<String> e = (Enumeration<String>) properties.propertyNames();
			for (; e.hasMoreElements();) {
				String key = e.nextElement();
				String val = properties.get(key).toString();
				// System.err.println(String.format("Added: \"%s\" = \"%s\"", key, val));
				propertiesMap.put(key, val);
			}
		} catch (IOException e) {
			System.err.println("Properties file was not found or not readable: " + e.toString());
		}

		return propertiesMap.get("value");
	}

	public String getValueConfig() {
		// System.err.println(valueConfig.getValueFromFile());
		return valueConfig.getValueFromFile();
	}
}
