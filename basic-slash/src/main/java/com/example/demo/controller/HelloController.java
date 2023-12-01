package com.example.demo.controller;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

import com.example.demo.utils.CreateFile;

@RestController
public class HelloController {

	@GetMapping("/hello")
	public String index() {
		CreateFile.append("controllerHello");
		return "Greetings from Spring Boot!";
	}

}