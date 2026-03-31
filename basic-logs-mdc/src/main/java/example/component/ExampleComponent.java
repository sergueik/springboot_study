package example.component;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Copyright 2026 Serguei Kouzmine
 */

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
public class ExampleComponent {
	private static final Logger log = LoggerFactory.getLogger(ExampleComponent.class);

	@Value("${example.property:default}")
	private String property;

	public String getProperty1() {
		log.info("hello");
		return property;
	}

}
