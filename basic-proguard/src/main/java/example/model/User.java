package example.model;

/**
 * Copyright 2025 Serguei Kouzmine
 */

import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import java.lang.IllegalArgumentException;

public class User {

	private Long id;
	@NotBlank(message = "Name is required")
	private String name;

	@NotBlank(message = "Email is required")
	@Email(message = "Email must be valid")
	private String email;

	public User() {
	}

	public User(Long id, String name, String email) {
		this.id = id;
		this.name = name;
		this.email = email;
	}

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		System.err.println(String.format("set  name: %s", (name == null ? "null" : name)));
		if (name.isEmpty())
			throw new IllegalArgumentException("name is required");
		this.name = name;
	}

	public String getEmail() {
		return email;
	}

	public void setEmail(String email) {
		this.email = email;
	}
}
