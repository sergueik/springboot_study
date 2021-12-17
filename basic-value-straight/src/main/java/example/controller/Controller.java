package example.controller;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.PropertySource;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Component;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import example.configuration.ValuesConfig;
import example.utils.Utils;

@Component
@RestController
@RequestMapping("/basic")
public class Controller {

	@Value("${value}")
	private String value;

	// @Autowired

	ValuesConfig valueConfig = new ValuesConfig();

	@GetMapping(value = "/nullvalue", produces = MediaType.TEXT_PLAIN_VALUE)
	public String nullValue() {
		// return "value: " + valueConfig.getValueFromFile();
		return value;
	}

	@GetMapping(value = "/value", produces = MediaType.TEXT_PLAIN_VALUE)
	public String value() {
		return new Utils().getValue();
	}
}
