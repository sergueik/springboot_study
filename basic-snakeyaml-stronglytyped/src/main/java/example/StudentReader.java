package example;

import org.yaml.snakeyaml.Yaml;
import org.yaml.snakeyaml.constructor.Constructor;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;
import java.util.Map;

import example.model.student.Student;

public class StudentReader {

	public void ReadYaml() {

		InputStream inputStream = this.getClass().getClassLoader()
				.getResourceAsStream("student.yml");
		Yaml yaml = new Yaml();
		Map<String, Object> data = yaml.load(inputStream);
		System.out.println(data);
	}

	public void readYamlWithCollection() {
		Yaml yaml = new Yaml();
		InputStream inputStream = this.getClass().getClassLoader()
				.getResourceAsStream("student_with_courses.yml");
		Map<String, Object> data = yaml.load(inputStream);
		System.out.println(data);
	}

	public void ReadYamlAsBean() {
		Yaml yaml = new Yaml(new Constructor(Student.class));
		InputStream inputStream = this.getClass().getClassLoader()
				.getResourceAsStream("student.yml");
		Student data = yaml.load(inputStream);
		System.out.println(data);
	}

	public void ReadYamlAsBeanWithNestedClass() {
		InputStream inputStream = this.getClass().getClassLoader()
				.getResourceAsStream("student_with_courses.yml");
		Yaml yaml = new Yaml(new Constructor(Student.class));
		Student data = yaml.load(inputStream);
		System.out.println(data);
	}

	public void FailedReadYamlAsBeanWithDeeplyNestedClasses() {
		InputStream inputStream = this.getClass().getClassLoader()
				.getResourceAsStream("student_with_courses_and_services.yml");
		
		try {
			System.out.println("reading YAML:\n" + readAll(inputStream));
		} catch (IOException e) {
			System.err.println("Exception:" + e.toString());
		}
		inputStream = this.getClass().getClassLoader()
				.getResourceAsStream("student_with_courses_and_services.yml");
		Yaml yaml = new Yaml(new Constructor(Student.class));
		Student data = yaml.load(inputStream);
		System.out.println(data);
	}

	public void ReadYamlAsBeanWithDeeplyNestedClasses() {
		InputStream inputStream = this.getClass().getClassLoader()
				.getResourceAsStream("student_with_courses_and_services_flattened.yml");
		try {
			System.out.println("reading YAML:\n" + readAll(inputStream));
		} catch (IOException e) {
			System.err.println("Exception:" + e.toString());
		}
		inputStream = this.getClass().getClassLoader()
				.getResourceAsStream("student_with_courses_and_services.yml");
		
		Yaml yaml = new Yaml(new Constructor(Student.class));
		Student data = yaml.load(inputStream);
		System.out.println(data);
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
