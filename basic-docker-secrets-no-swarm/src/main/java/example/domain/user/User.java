package example.domain.user;

public class User {
	String id;
	String login;
	String password;

	private User(UserBuilder builder) {
		this.id = builder.id;
		this.login = builder.login;
		this.password = builder.password;
	}

	public static UserBuilder builder() {
		return new UserBuilder();
	}

	public String getId() {
		return id;
	}

	public String getLogin() {
		return login;
	}

	public String getPassword() {
		return password;
	}

	// Builder Class
	public static class UserBuilder {
		// required parameters
		String id;
		String login;
		String password;

		public UserBuilder() {

		}

		public UserBuilder(String id, String login, String password) {
			this.id = id;
			this.login = login;
			this.password = password;
		}

		public UserBuilder id(String id) {
			this.id = id;
			return this;
		}

		public UserBuilder login(String login) {
			this.login = login;
			return this;
		}

		public UserBuilder password(String password) {
			this.password = password;
			return this;
		}

		public User build() {
			return new User(this);
		}
	}
}
