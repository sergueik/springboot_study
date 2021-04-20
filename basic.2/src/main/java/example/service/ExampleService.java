package example.service;

import org.springframework.stereotype.Component;
import org.springframework.stereotype.Service;

import example.controller.ExampleController;

// @Component
@Service
// NOTE: one should never use the class 'Service': collision with stereotype:
// incompatible types: example.Service cannot be converted to
// java.lang.annotation.Annotation

public class ExampleService {

	public String hello() {
		return "Hello basic";
	}

	public ExampleController.Data handleData(ExampleController.Data data) {
		return data;
	}
}