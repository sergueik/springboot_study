package example;

import org.yaml.snakeyaml.DumperOptions;
import org.yaml.snakeyaml.Yaml;

import example.model.student.Course;
import example.model.student.Student;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

public class StudentWriter {

	public void WriteYaml() throws IOException {
		Map<String, Object> dataMap = new HashMap<>();
		dataMap.put("id", 19);
		dataMap.put("name", "John");
		dataMap.put("address", "Star City");
		dataMap.put("department", "Medical");

		DumperOptions options = new DumperOptions();
		options.setIndent(2);
		options.setPrettyFlow(true);
		options.setDefaultFlowStyle(DumperOptions.FlowStyle.BLOCK);
		Yaml yaml = new Yaml(options);
		PrintWriter writer = new PrintWriter(
				new File("./src/main/resources/student_output.yml"));
		// StringWriter writer = new StringWriter();
		yaml.dump(dataMap, writer);
	}

	public void WriteYamlBasic() {

		DumperOptions options = new DumperOptions();
		options.setIndent(2);
		options.setPrettyFlow(true);
		options.setDefaultFlowStyle(DumperOptions.FlowStyle.BLOCK);
		Yaml yaml = new Yaml(options);

		StringWriter writer = new StringWriter();
		yaml.dump(basicStudentObject(), writer);
		System.out.println(writer.toString());
	}

	public void WriteYamlBasicWithCollection() throws FileNotFoundException {

		DumperOptions options = new DumperOptions();
		options.setIndent(2);
		options.setPrettyFlow(true);
		options.setDefaultFlowStyle(DumperOptions.FlowStyle.BLOCK);
		Yaml yaml = new Yaml(options);

		// StringWriter writer = new StringWriter();
		PrintWriter writer = new PrintWriter(
				new File("./src/main/resources/student_output_bean.yml"));
		yaml.dump(studentObject(), writer);
		System.out.println(writer.toString());
	}

	private Student basicStudentObject() {
		Student student = new Student();

		student.setId(21);
		student.setName("Tim");
		student.setAddress("Night City");
		student.setYear(2077);
		student.setDepartment("Cyberware");

		return student;
	}

	private Student studentObject() {
		Student student = new Student();

		student.setId(21);
		student.setName("Tim");
		student.setAddress("Night City");
		student.setYear(2077);
		student.setDepartment("Cyberware");

		Course courseOne = new Course();
		courseOne.setName("Intelligence");
		courseOne.setCredits(5);

		Course courseTwo = new Course();
		courseTwo.setName("Crafting");
		courseTwo.setCredits(2);

		List<Course> courseList = new ArrayList<>();
		courseList.add(courseOne);
		courseList.add(courseTwo);

		student.setCourses(courseList);

		return student;
	}
}
