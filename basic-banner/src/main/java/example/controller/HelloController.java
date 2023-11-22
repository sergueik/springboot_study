package example.controller;
/**
 * Copyright 2023 Serguei Kouzmine
 */

import java.util.HashMap;
import java.util.Map;

import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

@RestController
@RequestMapping("/basic")
public class HelloController {

	private static Gson gson = new GsonBuilder().create();

	@PostMapping(value = "/hello", consumes = {
			MediaType.APPLICATION_JSON_VALUE }, produces = {
					MediaType.APPLICATION_JSON_VALUE })
	public String hello(@RequestBody Map<String, String> data) {
		// echo back the request data
		return gson.toJson(data);
	}

}
