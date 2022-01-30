package example.model;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

import org.hibernate.validator.constraints.Email;
import org.hibernate.validator.constraints.NotEmpty;

public class User {

	@NotNull
	@Size(min = 2, max = 30, message = "Name should contains more than 2 character")
	private String name;

	@NotNull
	@Email(message = "Invalid Email")
	@Pattern(regexp = "\\b[\\w.%-]+@[-.\\w]+\\.[A-Za-z]{2,4}\\b", message = "Invalid Email")
	private String email;

	@NotNull(message = "may not be null")
	@NotEmpty(message = "* Mandatory field")
	@Size(min = 6, max = 8, message = "Character should be in between 6 to 8")
	private String password;

	public String getName() {
		return name;
	}

	public void setName(String data) {
		name = data;
	}

	public String getEmail() {
		return email;
	}

	public void setEmail(String data) {
		email = data;
	}

	public String getPassword() {
		return password;
	}

	public void setPassword(String data) {
		password = data;
	}

	@Override
	public String toString() {
		return "User [name=" + name + ", email=" + email + ", password=" + password
				+ "]";
	}

}
