package example.domain;

import io.swagger.annotations.ApiModelProperty;
public class Person {
	@ApiModelProperty(notes = "name")
	private String name;

	public String getName() {
		return name;
	}

	public void setName(String value) {
		name = value;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String value) {
		title = value;
	}

	public Long getSalary() {
		return salary;
	}

	public void setSalary(Long value) {
		salary = value;
	}

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
