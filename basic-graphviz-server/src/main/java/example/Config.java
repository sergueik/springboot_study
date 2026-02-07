package example;

import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

public class Config {
	private static final Properties properties = new Properties();

	static {
		try (InputStream inputStream = Config.class.getClassLoader().getResourceAsStream("application.properties")) {
			if (inputStream != null) {
				properties.load(inputStream);
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	/**
	 * Gets a configuration property with dual fallback:
	 * * check JVM system property (-Dkey=value)
	 * * try application.properties file
	 * * fallback to default value
	 */
	public static String get(String key, String defaultValue) {

		String property = System.getProperty(key, properties.getProperty(key, defaultValue)); // fallback
		return property;
	}
}
