package com.slm.proguard.example.spring.boot.controller;


import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.client.RestTemplate;


import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import com.slm.proguard.example.spring.boot.service.ExampleService;

@RestController
@RequestMapping("/example")
public class Controller {

	@Autowired
	private ExampleService service;

	public Controller(ExampleService data) {
		service = data;
	}

	@GetMapping(produces = { MediaType.TEXT_PLAIN_VALUE })
	public int calculate() {
		return service.calculate();
	}

	@GetMapping(value = "/error", produces = { MediaType.APPLICATION_JSON_VALUE })
	public void error() {
	}
}
