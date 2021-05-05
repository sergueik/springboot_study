package example.service;

import java.util.List;
import java.util.Map;

/**
 * Copyright 2021 Serguei Kouzmine
 */
import org.springframework.stereotype.Service;

import example.controller.ExampleRestController;

@Service
public class ExampleService {

	// do some processing
	public String hello() {
		return "Hello basic";
	}

	// do some more processing
	public Map<String, List<ExampleRestController.Data>> handleData(
			Map<String, List<ExampleRestController.Data>> results) {
		return results;
	}
}