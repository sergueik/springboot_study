package example;

import java.io.FileInputStream;
import java.util.Properties;

public class PropertyReader {

	private static volatile PropertyReader propInstance;

	private PropertyReader() {

	}

	public static synchronized PropertyReader getInstance() {
		if (propInstance == null) {
			propInstance = new PropertyReader();
		}
		return propInstance;
	}

	public String getProperty(String propertyName) {

		Properties prop = new Properties();
		FileInputStream inputStream = null;
		try {
			inputStream = new FileInputStream(System.getProperty("user.dir") + "/src/test/resources/config.properties");
			prop.load(inputStream);
			if (prop.getProperty(propertyName) != null) {
				return prop.getProperty(propertyName);
			}
		} catch (Exception e) {
			System.out.println("Property not found");
		}
		return null;
	}
}
