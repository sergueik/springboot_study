package example.resource;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import example.configs.ReloadableProperties;

import java.util.Properties;
import example.component.ExampleComponent;
import example.component.InjectablePropertiesComponent;
import example.component.PropertiesParser;

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

	public String getStaticValue() {
		final String value = ExampleComponent.examplesStaticProperty;
		return value == null ? "unknown" : value;
	}

	public String getValue() {
		final String value = properties.getProperty("value.property");
		return value == null ? "unknown" : value;
	}

	@Autowired
	InjectablePropertiesComponent injectablePropertiesComponent = new InjectablePropertiesComponent(properties);

	public String getInjectablePropertiesComponentInstanceValue() {
		// unknown
		final String value = injectablePropertiesComponent.getSomeProperty();
		return value == null ? "unknown" : value;
	}

	public String getPropertiesParserStaticValue() {
		final String value = PropertiesParser.getSomeProperty();
		return value == null ? "unknown" : value;
	}

	@Autowired
	private ExampleComponent exampleComponent;

	public String getInstanceValue() {
		// unknown
		final String value = /* new ExampleComponent() */ exampleComponent.getExampleInstanceProperty();
		return value == null ? "unknown" : value;
	}

	@GetMapping
	public String Hello() {
		return "Hello instance: " + getInstanceValue() + "\n" + "static: " + getStaticValue() + "\n" + "static (2): "
				+ getPropertiesParserStaticValue() + "\n" + "injectablePropertiesComponentInstanceValue: "
				+ getInjectablePropertiesComponentInstanceValue() + "\n" + "own: " + getValue();
	}

}
