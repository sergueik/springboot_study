package example;

import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.core.env.Environment;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.client.RestTemplate;

@RestController
public class Controller {
	@Autowired
	private Environment environment;

	final String hostname = System.getenv().getOrDefault("HOSTNAME", "unknown");
	String greeting;

	private int count = 0; // simple counter to see lifecycle

	RestTemplate restTemplate = new RestTemplate();

	@RequestMapping("/")
	public String sayHello() {
		greeting = environment.getProperty("GREETING", "Aloha");
		System.out.println(greeting + " from " + hostname);
		return greeting + " from Spring Boot! " + count++ + " on " + hostname
				+ "\n";
	}

	@RequestMapping("/crash")
	public String doCrash() {
		throw new IllegalAccessError();
	}

}