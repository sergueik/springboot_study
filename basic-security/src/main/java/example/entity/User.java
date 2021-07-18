package example.entity;

import example.dao.UserDetailsServiceDAO;

public class User {

	private String username;
	private String password;
	private String role;

	public User() {
	}

	public User(String username, String password) {
		this.password = password;
		this.username = username;
		this.role = UserDetailsServiceDAO.ROLE.USER.toString();
	}

	public String getUsername() {
		return username;
	}

	public void setUsername(String data) {
		username = data;
	}

	public String getPassword() {
		return password;
	}

	public void setPassword(String data) {
		password = data;
	}

	public String getRole() {
		return role;
	}

	public void setRole(String data) {
		role = data;
	}

}
