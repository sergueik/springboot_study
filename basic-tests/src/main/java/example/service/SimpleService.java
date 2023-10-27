package example.service;
/**
 * Copyright 2023 Serguei Kouzmine
 */

import org.springframework.stereotype.Service;

@Service
public class SimpleService {

	public SimpleService() {

	}

	public String hello() {
		return "Hello ";
	}

	public String hello(final String name) {
		return "Hello " + name;
	}

}