package example.integration;

import java.util.Properties;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import example.component.Example2Component;
import example.component.ExplicitPropertiesParser;

@RestController
@RequestMapping("/dummy")
public class DummyController {
	@Autowired
	private Properties properties;

	public DummyController(@Autowired Properties properties) {
		this.properties = properties;
	}

	public String getValue() {
		final String value = properties.getProperty("value.property");
		return value == null ? "unknown" : value;
	}

	@GetMapping
	public String Process() {
		return "Hello dummy: " + getValue();
	}

	public String getExplicitPropertyValue() {
		final String value = ExplicitPropertiesParser.getSomeProperty();
		return value == null ? "unknown" : value;
	}

	@GetMapping("/explicit")
	public String ProcessExplicitProperty() {
		return "Hello explicit property parser property: " + getValue();
	}

}
