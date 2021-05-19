package example.resource;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.Properties;
import example.component.Example1Component;
import example.component.Example2Component;
import example.component.PropertiesParser;

@Component
@RestController
// NOTE: cannot share the request mapping
@RequestMapping("/")
public class Worker {
	private boolean test = false;

	public void setTest(boolean value) {
		test = value;
	}

	private Properties properties;

	Example2Component example2;
	@Autowired
	private Example1Component example1;

	// TODO: public Worker(@Autowired ReloadableProperties properties)
	public Worker(@Autowired Properties properties) {
		this.properties = properties;
		example2 = new Example2Component(this.properties);
		// x = Example3Component.getInstance();
	}

	public String getStaticValue() {
		final String value = Example1Component.examplesStaticProperty;
		return value == null ? "unknown" : value;
	}

	public String getValue() {
		final String value = properties.getProperty("value.property");
		return value == null ? "unknown" : value;
	}

	public String getExample2PropertyValue() {
		// unknown
		final String value = example2.getSomeProperty();
		return value == null ? "unknown" : value;
	}

	public String getPropertiesParserStaticValue() {
		final String value = PropertiesParser.getSomeProperty();
		return value == null ? "unknown" : value;
	}

	public String getExample1PropertyValue() {
		// unknown
		final String value = example1.getExampleInstanceProperty();
		return value == null ? "unknown" : value;
	}

	// NOTE: commented the entries that lead to NPE in the test
	@GetMapping("/worker")
	public String Hello() {
		StringBuilder response = new StringBuilder();
		response.append("Hello: \n");
		if (!test)
			response.append("instance: " + getExample1PropertyValue() + "\n");
		response.append("static: " + getStaticValue() + "\n");
		response.append("static (2): " + getPropertiesParserStaticValue() + "\n");
		response.append("injected: " + getExample2PropertyValue() + "\n");
		response.append("own: " + getValue() + "\n");
		return response.toString();
	}

	@GetMapping("/basic")
	public String Process() {
		return "Hello basic";
	}

}
