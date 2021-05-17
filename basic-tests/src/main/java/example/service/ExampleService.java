package example.service;

/**
 * Copyright 2021 Serguei Kouzmine
 */

import org.springframework.beans.factory.annotation.Autowired;

import org.springframework.stereotype.Component;
import org.springframework.stereotype.Service;

import com.github.sergueik.example.ExampleComponent;

import example.controller.ExampleController;


@Service
// NOTE: one should never name the class 'Service': collision with stereotype:
// incompatible types: example.Service cannot be converted to
// java.lang.annotation.Annotation

public class ExampleService {

	@Autowired
	private ExampleComponent propertyComponent;

	public String hello() {
		return "Hello " + propertyComponent.getProperty1();
	}

	public ExampleController.Data handleData(ExampleController.Data data) {
		return data;
	}
}