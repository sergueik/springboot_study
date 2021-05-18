package example.component;

/**
 * Copyright 2021 Serguei Kouzmine
 */

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import example.utils.Utils;

@Component
public class Example1Component {
	private static String osName = Utils.getOSName();

	@Value("${example.Property1:property1 default value}")
	private String property1;

	@Value("${example.Property2:property2 default value}")
	private String property2;

	public String getExampleInstanceProperty() {
		if (osName.equals("windows"))
			return property1;
		else
			return property2;

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
		Example1Component.examplesStaticProperty = name;
	}

}
