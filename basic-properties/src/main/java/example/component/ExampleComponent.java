package example.component;

/**
 * Copyright 2021 Serguei Kouzmine
 */

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
public class ExampleComponent {
	@Value("${example.InstanceProperty:default value instance property}")
	private String exampleInstanceProperty;

	public String getExampleInstanceProperty() {
		return exampleInstanceProperty;
	}

	public static String examplesStaticProperty;

	// https://www.baeldung.com/spring-inject-static-field
	// create an transfer instance variable for each static one likes to value
	// inject
	@Value("${example.StaticProperty:default value of static property}")
	private String propertyValueProvider;

	// repeat the annotation verbatim
	@Value("${example.StaticProperty:default value of static property}")
	public void setExampleStaticProperty(String name) {
		ExampleComponent.examplesStaticProperty = name;
	}

}
