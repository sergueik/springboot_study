package example;

import org.yaml.snakeyaml.Yaml;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.Map;
import example.model.Writer;

/**
 * based on: https://github.com/TaimoorChoudhary/snake-yaml
 */
public class Student {

	static Reader reader = new Reader();
	static Writer writer = new Writer();

	public static void main(String[] args) {
		System.out.println("***** SNAKE-YAML *****\n");
		System.out.println("**** READER ****\n");
		System.out.println("*** Read YAML ***");
		readYaml();

		System.out.println("\n*** Read YAML containing Collection ***");
		readYamlWithCollection();

		System.out.println("\n*** Read YAML as Bean ***");
		readYamlAsBean();

		System.out.println("\n*** Read YAML as Bean with Nested Class ***");
		readYamlAsBeanWithNestedClass();

		System.out.println("\n**** WRITER ****\n");
		System.out.println("*** Write YAML ***");
		writeYaml();

		System.out.println("*** Write YAML Basic ***");
		writeYamlBasic();

		System.out.println("*** Write YAML with Collection ***");
		writeYamlCollection();
	}

	/**
	 * Read Basic YAML File
	 */
	private static void readYaml() {
		reader.ReadYaml();
	}

	private static void readYamlWithCollection() {
		reader.readYamlWithCollection();
	}

	private static void readYamlAsBean() {
		reader.ReadYamlAsBean();
	}

	private static void readYamlAsBeanWithNestedClass() {
		reader.ReadYamlAsBeanWithNestedClass();
	}

	private static void writeYaml() {
		try {
			writer.WriteYaml();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	private static void writeYamlBasic() {
		writer.WriteYamlBasic();
	}

	private static void writeYamlCollection() {
		try {
			writer.WriteYamlBasicWithCollection();
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		}
	}
}
