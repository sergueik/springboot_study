package example;

import org.yaml.snakeyaml.Yaml;
import org.yaml.snakeyaml.constructor.Constructor;
import org.yaml.snakeyaml.LoaderOptions;

import java.io.InputStream;
import java.io.Reader;
import java.io.StringReader;

public class CopyBookYamlLoader {
	private static final LoaderOptions LOADER_OPTIONS = new LoaderOptions();

	private static final Yaml YAML = new Yaml(new Constructor(CopyBookSpec.class,LOADER_OPTIONS));

	private CopyBookYamlLoader() {
	}

	public static CopyBookSpec loadFromResource(String resourcePath) {
		InputStream inputStream = CopyBookYamlLoader.class.getClassLoader().getResourceAsStream(resourcePath);

		if (null == inputStream) {
			throw new IllegalArgumentException("Resource not found: " + resourcePath);
		}
		return YAML.load(inputStream);
	}

	public static CopyBookSpec loadFromString(String data) {
		return YAML.load(new StringReader(data));
	}

}