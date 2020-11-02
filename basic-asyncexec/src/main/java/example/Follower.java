package example;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class Follower {

	private String type;
	private String login;

	public String getType() {
		return type;
	}

	public void setType(String data) {
		type = data;
	}

	public String getLogin() {
		return login;
	}

	public void setLogin(String data) {
		login = data;
	}

	@Override
	public String toString() {
		return "User [login=" + login + ", type=" + type + "]";
	}

}
