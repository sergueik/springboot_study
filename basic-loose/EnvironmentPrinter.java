package example;

// based on: http://www.java2s.com/Tutorial/Java/0120__Development/GetEnvironmentVariables.htm
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

public class EnvironmentPrinter {

	public void printProperties() {
		Map<Object, Object> map = System.getProperties();
		Set<Object> keys = map.keySet();
		Iterator<Object> iterator = keys.iterator();
		while (iterator.hasNext()) {
			String key = (String) iterator.next();
			String value = (String) map.get(key);

			System.err.println(key + " = " + value);
			// invoke again
			System.err.println(key + " = " + System.getProperty(key));
		}
	}

	public void printEnvironment() {

		Map<String, String> map = System.getenv();

		Set<String> keys = map.keySet();
		Iterator<String> iterator = keys.iterator();
		while (iterator.hasNext()) {
			String key = iterator.next();
			String value = map.get(key);

			System.err.println(key + " = " + value);
			// invoke again
			System.err.println(key + " = " + System.getenv(key));
		}
	}

	// origin:
	// https://github.com/TsvetomirSlavov/wdci/blob/master/code/src/main/java/com/seleniumsimplified/webdriver/manager/EnvironmentPropertyReader.java
	public static String getPropertyEnv(String name, String defaultValue) {
		String value = System.getProperty(name);
		if (value == null || value.length() == 0) {
			value = System.getenv(name);
			if (value == null || value.length() == 0) {
				value = defaultValue;
			}
		}
		return value;
	}

	public static void main(String[] args) {
		if (args.length == 0) {
			(new EnvironmentPrinter()).printEnvironment();
			(new EnvironmentPrinter()).printProperties();
		} else {
			String key = args[0];
			System.err.println("Environment " + key + " = " + System.getenv(key));
			System.err.println("Property " + key + " = " + System.getProperty(key));

		}
	}
}
