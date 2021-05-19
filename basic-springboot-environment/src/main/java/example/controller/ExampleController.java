package example.controller;

import java.util.Properties;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/")
public class ExampleController {

	// optional?
	private Properties properties;

	public ExampleController(@Autowired Properties properties) {
		this.properties = properties;
	}

	@GetMapping("/environment/{key}")
	public String environmenCheck(@PathVariable("key") String key) {
		StringBuilder response = new StringBuilder();
		response.append("Environment " + key + ": " + System.getenv(key) + "\n");
		return response.toString();
	}

	@Value("${somevalue:unknown}")
	private String somevalue;

	@GetMapping("/property/{key}")
	public String propertyCheck(@PathVariable("key") String key) {
		StringBuilder response = new StringBuilder();
		response.append("System Property " + key + ": "
				+ System.getProperty(key, "unknown") + "\n");
		if (key.equals("somevalue")) {
			response.append("Application Property " + key + ": " + somevalue + "\n");
		}
		return response.toString();
	}
}
