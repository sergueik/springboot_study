package example;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.ConfigurableApplicationContext;

import example.configuration.VaultConfiguration;

@SpringBootApplication
public class Application {

	public static void main(String[] args) {
		ConfigurableApplicationContext context = SpringApplication
				.run(Application.class, args);
		VaultConfiguration vaultConfiguration = context
				.getBean(VaultConfiguration.class);
		System.out.println("Login: " + vaultConfiguration.getLogin());
		System.out.println("Password: " + vaultConfiguration.getPassword());
	}

}
