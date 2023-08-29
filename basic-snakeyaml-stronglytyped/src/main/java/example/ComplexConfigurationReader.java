package example;

// import static org.hamcrest.CoreMatchers.notNullValue;
//import static org.hamcrest.MatcherAssert.assertThat;

import java.io.IOException;
import java.io.InputStream;

import org.yaml.snakeyaml.LoaderOptions;
import org.yaml.snakeyaml.Yaml;
import org.yaml.snakeyaml.constructor.Constructor;
import java.lang.NullPointerException;
import java.util.Map;

import example.model.configuration.ComplexConfiguration;

public class ComplexConfigurationReader {

	public void ReadYamlAsBeanWithNestedClass() {
		InputStream inputStream = this.getClass().getClassLoader()
				.getResourceAsStream("complex_config.yml");
		try {
			System.out.println("reading YAML:\n" + readAll(inputStream));
		} catch (IOException e) {
			System.err.println("Exception:" + e.toString());
		}
		inputStream = this.getClass().getClassLoader()
				.getResourceAsStream("complex_config.yml");
		Yaml yaml = new Yaml(new Constructor(ComplexConfiguration.class));
		ComplexConfiguration data = yaml.load(inputStream);

		System.out.println("Read data: " + data);
		try {
			System.out.println("Read data version: " + data.getVersion());
		} catch (NullPointerException e) {
			System.err.println("Exception:" + e.toString());
		}
		// https://bitbucket.org/snakeyaml/snakeyaml/wiki/Documentation
		// for debug dump, load YAML untyped
		final LoaderOptions options = new LoaderOptions();
		Yaml yaml1 = new Yaml(options);
		inputStream = this.getClass().getClassLoader()
				.getResourceAsStream("complex_config.yml");
		Map<String, Object> object1 = (Map<String, Object>) yaml1.load(inputStream);
		System.err.println("Object: " + object1);

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
