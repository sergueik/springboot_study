package example.integration;

import java.util.Properties;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/dummy")
public class DummyWorker {
	@Autowired
	private Properties properties;

	public String getValue() {
		final String value = properties.getProperty("value.property");
		return value == null ? "unknown" : value;
	}

	@GetMapping
	public String Process() {
		return "Hello dummy";
		// + getValue();
	}

}
