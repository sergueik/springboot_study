package example.model;
/**
 * Copyright 2025 Serguei Kouzmine
 */

import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;

public class User {
	
    private Long id;
    @NotBlank
    private String name;
    @Email
    private String email;

    public User() {}
    public User(Long id, String name, String email) {
        this.id = id;
        this.name = name;
        this.email = email;
    }

    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }

    public String getName() { return name; }
    public void setName(String name) { this.name = name; }

    public String getEmail() { return email; }
    public void setEmail(String email) { this.email = email; }
}

