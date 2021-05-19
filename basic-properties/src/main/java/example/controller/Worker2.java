package example.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.Properties;
import example.component.Example1Component;
import example.component.Example2Component;
import example.component.Example3Component;
import example.component.ExplicitPropertiesParser;

@RestController
@RequestMapping("/")
public class Worker2 {

	@Autowired
	Properties properties;
	@Autowired
	private final Example3Component component = new Example3Component(properties);

	@GetMapping("/environment-check")
	public String environmenCheck() {
		StringBuilder response = new StringBuilder();
		response.append("Environment check: " + System.getenv("somevalue") + "\n");
		return response.toString();
	}

	@GetMapping("/property-check")
	public String propertyCheck() {
		StringBuilder response = new StringBuilder();
		response.append(
				"Derived property # 1: " + component.getDerivedProperty1() + "\n");
		response.append("Value property # 1: " + component.getProperty1() + "\n");
		response.append(
				"Derived property # 2: " + component.getDerivedProperty2() + "\n");
		response.append("Value property # 2: " + component.getProperty2() + "\n");
		response.append("Value of static property: "
				+ ExplicitPropertiesParser.getSomeProperty() + "\n");
		return response.toString();
	}

}
