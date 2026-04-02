package example.service;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Copyright 2021,2023 Serguei Kouzmine
 */

import org.springframework.beans.factory.annotation.Autowired;

import org.springframework.stereotype.Component;
import org.springframework.stereotype.Service;

import example.component.ExampleComponent;
import example.controller.ExampleController;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Copyright 2026 Serguei Kouzmine
 */

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Service
public class ExampleService {
	private static final Logger log = LoggerFactory.getLogger(ExampleService.class);

	@Autowired
	private ExampleComponent propertyComponent;

	@Autowired
	public ExampleService(ExampleComponent data) {
		propertyComponent = data;
	}

	public ExampleService() {
	}

	public String hello() {
		log.info("hello");
		return "Hello " + propertyComponent.getProperty1();
	}

}
