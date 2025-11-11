package example.model;

/**
 * Copyright 2025 Serguei Kouzmine
 */

import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;

public class User {

	@NotBlank(message = "Name cannot be blank")
	private String name;

	@NotBlank(message = "Email cannot be blank")
	@Email(message = "Email should be valid")
	private String email;

	@NotNull(message = "Age is required")
	private Integer age;

	@Size(min = 10, max = 15, message = "Phone number must be between 10 and 15 chars")
	private String phone;

	public User() {
	}

	public User(String name, String email, Integer age, String phone) {
		this.name = name;
		this.email = email;
		this.age = age;
		this.phone = phone;
	}

	// Getters and setters

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getEmail() {
		return email;
	}

	public void setEmail(String email) {
		this.email = email;
	}

	public Integer getAge() {
		return age;
	}

	public void setAge(Integer age) {
		this.age = age;
	}

	public String getPhone() {
		return phone;
	}

	public void setPhone(String phone) {
		this.phone = phone;
	}
}
