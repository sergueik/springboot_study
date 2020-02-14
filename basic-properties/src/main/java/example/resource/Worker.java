package example.resource;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.Properties;


@Component
@RestController
@RequestMapping("/basic")
public class Worker {
	private Properties properties;

	public Worker(@Autowired Properties properties) {
		this.properties = properties;
	}

	public String getValue() {
		return properties.getProperty("application.property");
	}

	@GetMapping
	public String Hello() {
		return "Hello " + getValue();
	}

}
