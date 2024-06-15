package example.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.env.Environment;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/")
public class MessageResource {

	@Autowired
	private Environment environment;
	@Value("${environment.name}")
	String environment_name;

	@Value("${server.port}")
	String port;

	@GetMapping("/message")
	public String getMessage() {
		return environment.getProperty("environment.name") + " " + environment_name + " " + " Running on Port Number "
				+ port;
	}

}
