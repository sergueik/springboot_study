package example;

import org.yaml.snakeyaml.Yaml;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.Map;

import example.StudentWriter;

/**
 * based on: https://github.com/TaimoorChoudhary/snake-yaml
 */
public class Main {

	static ConfigurationReader configurationReader = new ConfigurationReader();
	static ComplexConfigurationReader complexConfigurationReader = new ComplexConfigurationReader();
	static StudentReader studentReader = new StudentReader();
	static StudentWriter writer = new StudentWriter();

	public static void main(String[] args) {

		System.out.println("***** SNAKE-YAML *****\n");
		System.out.println("**** CONFIGURATION READER ****\n");
		readConfigurationYamlAsBeanWithNestedClass();

		System.out.println("***** SNAKE-YAML *****\n");
		System.out.println("**** COMPLEX CONFIGURATION READER ****\n");
		readComplexConfigurationYamlAsBeanWithNestedClass();
		// return;

		/*
		System.out.println("***** SNAKE-YAML *****\n");
		System.out.println("**** READER ****\n");
		System.out.println("*** Read YAML ***");
		readYaml();
		
		System.out.println("\n*** Read YAML containing Collection ***");
		readYamlWithCollection();
		
		System.out.println("\n*** Read YAML as Bean ***");
		readYamlAsBean();
		*/
		System.out.println("\n*** Read YAML as Bean with Nested Class ***");
		readStudentYamlAsBeanWithNestedClass();

		/*
				System.out.println("\n**** WRITER ****\n");
				System.out.println("*** Write YAML ***");
				writeYaml();
		
				System.out.println("*** Write YAML Basic ***");
				writeYamlBasic();
		
				System.out.println("*** Write YAML with Collection ***");
				writeYamlCollection();
				*/
	}

	/**
	 * Read Basic YAML File
	 */
	private static void readYaml() {
		studentReader.ReadYaml();
	}

	private static void readYamlWithCollection() {
		studentReader.readYamlWithCollection();
	}

	private static void readYamlAsBean() {
		studentReader.ReadYamlAsBean();
	}

	private static void readStudentYamlAsBeanWithNestedClass() {
		studentReader.ReadYamlAsBeanWithNestedClass();
	}

	private static void readConfigurationYamlAsBeanWithNestedClass() {
		configurationReader.ReadYamlAsBeanWithNestedClass();
	}

	private static void readComplexConfigurationYamlAsBeanWithNestedClass() {
		complexConfigurationReader.ReadYamlAsBeanWithNestedClass();
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
