package example.component;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

import example.configuration.VaultConfiguration;

@Component
public class TaskComponent {
	@Autowired
	VaultConfiguration vaultConfiguration;

	@EventListener(ApplicationReadyEvent.class)
	public void ready() {
		System.out.println("Login: " + vaultConfiguration.getLogin());
		System.out.println("Password: " + vaultConfiguration.getPassword());

	}
}
