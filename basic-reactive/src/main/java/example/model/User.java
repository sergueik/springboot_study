package example.model;

import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.TypeAlias;
import org.springframework.data.mongodb.core.mapping.Document;

import javax.validation.constraints.*;
import java.io.Serializable;

@SuppressWarnings("serial")
@Document(collection = "user")
@TypeAlias("user")
public class User implements Serializable {

	@Id
	private String id;

	@NotNull
	@NotEmpty
	private String name;

	@NotEmpty
	@Email
	private String email;

	protected User() {
	}

	public User(String name, String email) {
		this();
		this.name = name;
		this.email = email;
	}

	public String getId() {
		return id;
	}

	public String getName() {
		return name;
	}

	public String getEmail() {
		return email;
	}
	public void setId(String data) {
		id = data;
	}

	public void setName(String data) {
		name = data;
	}

	public void setEmail(String data) {
		email = data;
	}

}
