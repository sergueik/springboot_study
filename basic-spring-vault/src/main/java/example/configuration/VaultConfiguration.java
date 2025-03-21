package example.configuration;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;

@Configuration
public class VaultConfiguration {
	@Value("${login}")
	public String login;

	@Value("${password}")
	public String password;

	public String getLogin() {
		return login;
	}

	public void setLogin(String value) {
		login = value;
	}

	public String getPassword() {
		return password;
	}

	public void setPassword(String value) {
		password = value;
	}
}
