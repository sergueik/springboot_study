package example;

import java.io.IOException;
import java.io.InputStream;

import org.yaml.snakeyaml.Yaml;
import org.yaml.snakeyaml.constructor.Constructor;
import java.lang.NullPointerException;
import example.model.configuration.Configuration;

public class ConfigurationReader {

	public void ReadYamlAsBeanWithNestedClass() {
		InputStream inputStream = this.getClass().getClassLoader()
				.getResourceAsStream("config.yml");
		try {
			System.out.println("raw YAML:\n" + readAll(inputStream));
		} catch (IOException e) {
			System.err.println("Exception:" + e.toString());
			return;
		}
		inputStream = this.getClass().getClassLoader()
				.getResourceAsStream("config.yml");
		Yaml yaml = new Yaml(new Constructor(Configuration.class));
		Configuration data = yaml.load(inputStream);

		System.out.println("Read data: " + data);
		try {
			System.out.println("Read data version: " + data.getVersion());
		} catch (NullPointerException e) {
			System.err.println("Exception:" + e.toString());
		}
	}

	// http://www.java2s.com/example/java-utility-method/inputstream-read-all/readall-inputstream-in-c5be9.html
	public static final String readAll(InputStream inputStream)
			throws IOException {
		StringBuilder stringBuilder = new StringBuilder();
		int c;
		while ((c = inputStream.read()) != -1) {
			stringBuilder.append((char) c);
		}
		// Exception:java.io.IOException: mark/reset not supported
		try {
			inputStream.reset();
		} catch (IOException e) {
			System.err.println("Exception:" + e.toString());
		}

		return stringBuilder.toString();
	}

}
