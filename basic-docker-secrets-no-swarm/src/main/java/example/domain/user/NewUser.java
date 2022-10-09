package example.domain.user;


public class NewUser {
	String login;
	String password;

	private NewUser(NewUserBuilder builder) {
		this.login = builder.login;
		this.password = builder.password;
	}

	public static NewUserBuilder builder() {
		return new NewUserBuilder();
	}

	public String getLogin() {
		return login;
	}

	public String getPassword() {
		return password;
	}

	// Builder Class
	public static class NewUserBuilder {
		// required parameters
		String login;
		String password;

		public NewUserBuilder() {

		}

		public NewUserBuilder(String login, String password) {
			this.login = login;
			this.password = password;
		}

		public NewUserBuilder login(String login) {
			this.login = login;
			return this;
		}

		public NewUserBuilder password(String password) {
			this.password = password;
			return this;
		}

		public NewUser build() {
			return new NewUser(this);
		}
	}

}
