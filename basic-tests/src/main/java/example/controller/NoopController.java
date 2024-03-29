package example.controller;
/**
 * Copyright 2021,2022,2023 Serguei Kouzmine
 */

import java.util.HashMap;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import example.service.ExampleService;

@RestController
@RequestMapping("/basic")
public class NoopController {

	private static Gson gson = new GsonBuilder().create();
	private boolean debug = false;
	private Map<String, String> result = new HashMap<>();

	@Autowired
	private ExampleService service;

	@Autowired
	public NoopController(ExampleService value) {
		service = value;
	}

	public NoopController() {

	}

	@PostMapping(value = "/hello", consumes = {
			MediaType.APPLICATION_JSON_VALUE }, produces = {
					MediaType.APPLICATION_JSON_VALUE })
	public String hello(@RequestBody Map<String, String> data, boolean noop) {
		// call the service unless noop. otherwise echo back the request data
		if (noop)
			return gson.toJson(data);
		else {
			result.clear();
			result.put("service response", service.hello());
			return gson.toJson(result);
		}
	}

	// NOTE: @PathVariable can not be Optional
	@PostMapping(value = "/hello/{noop}", consumes = {
			MediaType.APPLICATION_JSON_VALUE }, produces = {
					MediaType.APPLICATION_JSON_VALUE })
	public String hello2(@RequestBody Map<String, String> data,
			@PathVariable boolean noop) {
		return hello(data, noop);
	}

	@PostMapping(value = "/hello/noop", consumes = {
			MediaType.APPLICATION_JSON_VALUE }, produces = {
					MediaType.APPLICATION_JSON_VALUE })
	public String hello(@RequestBody Map<String, String> data) {
		return hello(data, true);
	}
}
