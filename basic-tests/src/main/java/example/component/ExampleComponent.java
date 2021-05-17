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
