package example.domain;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

@Data
public class Person {
	@ApiModelProperty(notes = "name")
	private String name;

	@ApiModelProperty(notes = "title")
	private String title;

	@ApiModelProperty(notes = "salary")
	private Long salary;

	public Person() {
	}

	public Person(String name, String title, Long salary) {
		this.name = name;
		this.title = title;
		this.salary = salary;
	}
}
