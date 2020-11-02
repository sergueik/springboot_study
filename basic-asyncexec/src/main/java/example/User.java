package example;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class User {

	private String name;
	private String login;
	private String blog;

	public String getName() {
		return name;
	}

	public void setName(String data) {
		name = data;
	}

	public String getLogin() {
		return login;
	}

	public void setLogin(String data) {
		login = data;
	}

	public String getBlog() {
		return blog;
	}

	public void setBlog(String data) {
		blog = data;
	}

	@Override
	public String toString() {
		return "User [name=" + name + ", blog=" + blog + "]";
	}

}
