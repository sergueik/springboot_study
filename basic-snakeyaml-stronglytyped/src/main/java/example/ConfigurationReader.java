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
				.getResourceAsStream("task_config.yml");
		try {
			System.out.println("reading YAML:" + readAll(inputStream));
		} catch (IOException e) {
			System.err.println("Exception:" + e.toString());
		}
		Yaml yaml = new Yaml(new Constructor(Configuration.class));
		Configuration data = yaml.load(inputStream);

		System.out.println("Read data: " + data);
		try {
			System.out.println("Read data version: " + data.getSettings());
		} catch (NullPointerException e) {
			System.err.println("Exception:" + e.toString());
		}
	}

	// http://www.java2s.com/example/java-utility-method/inputstream-read-all/readall-inputstream-in-c5be9.html
	public static final String readAll(InputStream in) throws IOException {
		StringBuilder bob = new StringBuilder();
		int c;
		while ((c = in.read()) != -1) {
			bob.append((char) c);
		}
		return bob.toString();
	}

}
