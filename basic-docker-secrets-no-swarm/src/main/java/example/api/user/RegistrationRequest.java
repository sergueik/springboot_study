package example.api.user;

class RegistrationRequest {
	String login;
	String password;

	public RegistrationRequest(String login, String password) {
		this.login = login;
		this.password = password;
	}

	public String getLogin() {
		return login;
	}

	public String getPassword() {
		return password;
	}

}
