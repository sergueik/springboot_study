package example.resource;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import example.configs.ReloadableProperties;

import java.util.Properties;

@Component
@RestController
// NOTE: cannot share the request mapping
@RequestMapping("/worker")
public class Worker {
	private Properties properties;

	// TODO: public Worker(@Autowired ReloadableProperties properties)
	public Worker(@Autowired Properties properties) {
		this.properties = properties;
	}

	public String getValue() {
		final String value = properties.getProperty("value.property");
		return value == null ? "unknown" : value;
	}

	@GetMapping
	public String Hello() {
		return "Hello " + getValue();
	}

}
