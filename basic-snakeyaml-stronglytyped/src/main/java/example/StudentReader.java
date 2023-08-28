package example;

import org.yaml.snakeyaml.Yaml;
import org.yaml.snakeyaml.constructor.Constructor;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
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
}
