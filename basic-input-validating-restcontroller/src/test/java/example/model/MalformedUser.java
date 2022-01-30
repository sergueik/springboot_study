package example.model;

public class MalformedUser {

	private String name;
	private String email;
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

	public MalformedUser() {
	}

	public MalformedUser(String name, String email, String password) {
		this.name = name;
		this.email = email;
		this.password = password;
	}

	@Override
	public String toString() {
		return "User [name=" + name + ", email=" + email + ", password=" + password
				+ "]";
	}

}
