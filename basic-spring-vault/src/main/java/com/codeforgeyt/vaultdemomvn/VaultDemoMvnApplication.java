package com.codeforgeyt.vaultdemomvn;

import com.codeforgeyt.vaultdemomvn.configuration.VaultConfiguration;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.ConfigurableApplicationContext;

@SpringBootApplication
public class VaultDemoMvnApplication {

	public static void main(String[] args) {
		ConfigurableApplicationContext context = SpringApplication.run(VaultDemoMvnApplication.class, args);
		VaultConfiguration vaultConfiguration = context.getBean(VaultConfiguration.class);
		System.out.println("Login: " + vaultConfiguration.getLogin());
		System.out.println("Password: " + vaultConfiguration.getPassword());
	}

}
