package example.service;

/**
 * Copyright 2021,2023 Serguei Kouzmine
 */

import org.springframework.beans.factory.annotation.Autowired;

import org.springframework.stereotype.Component;
import org.springframework.stereotype.Service;

import example.component.ExampleComponent;
import example.controller.ExampleController;

@Service
// NOTE: one should never name the class 'Service': collision with stereotype:
// incompatible types: example.Service cannot be converted to
// java.lang.annotation.Annotation

public class ExampleService {

	@Autowired
	private ExampleComponent propertyComponent;

	@Autowired
	public ExampleService(ExampleComponent data) {
		propertyComponent = data;
	}

	public ExampleService() {

	}

	public String hello() {
		return "Hello " + propertyComponent.getProperty1();
	}

	public String hello(final String name) {
		return "Hello " + name;
	}

	public ExampleController.Data handleData(ExampleController.Data data) {
		return data;
	}

	public ExampleController.Data handleData(Object data) {
		return null;
	}
}