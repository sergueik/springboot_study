package example.component;

/**
 * Copyright 2021 Serguei Kouzmine
 */

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
public class ExampleComponent {
	@Value("${example.Property1:default}")
	private String property1;
	@Value("${example.Property2:default}")
	private String property2;
	@Value("${example.Property3:/tmp}")
	private String property3;
	@Value("${example.Property4:c:\\temp}")
	private String property4;
	@Value("${example.Property5:default}")
	public String property5;

	public static String property6Static;

	// https://www.baeldung.com/spring-inject-static-field
	// create an instance variable for each static one likes to value inject
	@Value("${example.Property6:default}")
	public String property6;

	@Value("${example.Property6:default}")
	public void setProperty6Static(String name) {
		ExampleComponent.property6Static = name;
	}

	public String getProperty1() {
		return property1;
	}

	public String getProperty2() {
		return property2;
	}

	public String getProperty3() {
		return property3;
	}

	public String getProperty4() {
		return property4;
	}
}
