package example;

import java.io.FileInputStream;
import java.io.FileNotFoundException;

// import static org.hamcrest.CoreMatchers.notNullValue;
//import static org.hamcrest.MatcherAssert.assertThat;

import java.io.IOException;
import java.io.InputStream;

import org.yaml.snakeyaml.DumperOptions;
import org.yaml.snakeyaml.LoaderOptions;
import org.yaml.snakeyaml.Yaml;
import org.yaml.snakeyaml.constructor.Constructor;

import java.lang.NullPointerException;
import java.util.HashMap;
import java.util.Map;

import example.model.configuration.ComplexConfiguration;

public class ComplexConfigurationReader {

	private final String resourceFilepath = System.getProperty("user.dir")
			+ System.getProperty("file.separator") + "target"
			+ System.getProperty("file.separator") + "classes"
			+ System.getProperty("file.separator");
	private final String propertiesFileName = "complex_config.yml";

	public void ReadYamlAsBeanWithNestedClass() {
		InputStream inputStream = this.getClass().getClassLoader()
				.getResourceAsStream(propertiesFileName);
		try {
			System.out.println(
					"reading YAML from: " + (resourceFilepath + propertiesFileName));
			inputStream = new FileInputStream(resourceFilepath + propertiesFileName);
		} catch (FileNotFoundException e) {
			System.err.println("Exception:" + e.toString());
			return;
		}
		try {
			System.out.println("raw YAML:\n" + readAll(inputStream));
		} catch (IOException e) {
			System.err.println("Exception:" + e.toString());
			return;
		}
		inputStream = this.getClass().getClassLoader()
				.getResourceAsStream("complex_config.yml");
		Yaml yaml = new Yaml(new Constructor(ComplexConfiguration.class));
		ComplexConfiguration data = yaml.load(inputStream);

		System.out.println("Read data: " + data);
		try {
			System.out.println("Read data version: " + data.getVersion());
			System.out.println("Read data extradata for Windows: "
					+ data.getExtradata().get("windows"));
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
		DumperOptions dumperOptions = new DumperOptions();
		dumperOptions.setIndent(2);
		dumperOptions.setPrettyFlow(true);
		dumperOptions.setDefaultFlowStyle(DumperOptions.FlowStyle.BLOCK);
		Yaml yaml3 = new Yaml(dumperOptions);

		ComplexConfiguration.Extraconfig extraconfig1 = new ComplexConfiguration.Extraconfig();
		extraconfig1.setHomedir("windows home dir");
		extraconfig1.setSysdir("windows sys dir");
		ComplexConfiguration.Extraconfig extraconfig2 = new ComplexConfiguration.Extraconfig();
		extraconfig2.setHomedir("linux home dir");
		extraconfig2.setSysdir("linux sys dir");

		Map<String, ComplexConfiguration.Extraconfig> configpaths = new HashMap<String, ComplexConfiguration.Extraconfig>();
		configpaths.put("windows", extraconfig1);
		configpaths.put("linux", extraconfig2);
		data.setConfigpaths(configpaths);
		String output = yaml3.dump(data);
		System.err.println("test3 result (with indentation):" + "\n" + output);
	}

	// http://www.java2s.com/example/java-utility-method/inputstream-read-all/readall-inputstream-in-c5be9.html
	// NOTE: caller needs to reset or reinitialize the inputStream
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

	protected static String getScriptContent(String scriptName) {
		try {
			final InputStream stream = ComplexConfigurationReader.class
					.getClassLoader().getResourceAsStream(scriptName);
			final byte[] bytes = new byte[stream.available()];
			stream.read(bytes);
			stream.close();
			return new String(bytes, "UTF-8");
		} catch (IOException e) {
			throw new RuntimeException(scriptName);
		}
	}

}
