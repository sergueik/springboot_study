package example.application;

import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.ImportResource;

@Configuration
@ImportResource({ "classpath*:applicationContext.xml" })
public class Actor {

	private String name;
	private String gender;
	private int age;

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getGender() {
		return gender;
	}

	public void setGender(String gender) {
		this.gender = gender;
	}

	public int getAge() {
		return age;
	}

	public void setAge(int age) {
		this.age = age;
	}

	@Override
	public String toString() {
		return "Actor{" + "name=" + "\"" + name + "\"" + ", gender=" + "\"" + gender
				+ "\"" + ", age=" + age + "}";
	}
}