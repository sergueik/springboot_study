package example.integration;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import example.component.ExplicitPropertiesParser;

@RestController
@RequestMapping("/dummy")
public class ExplicitPropertiesParserController {

	public String getValue() {
		final String value = ExplicitPropertiesParser.getSomeProperty();
		return value == null ? "unknown" : value;
	}

	@GetMapping("/explicit")
	public String Process() {
		return "value: " + getValue();
	}

}
