package example.controller;

/**
 * Copyright 2021 Serguei Kouzmine
 */

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.util.MultiValueMap;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import example.service.ExampleService;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonSerializationContext;
import com.google.gson.JsonSerializer;

@RestController
@RequestMapping("/data")
public class ExampleRestController {

	private static Gson gson = new Gson();
	@Autowired
	private ExampleService service;

	public ExampleRestController(ExampleService data) {
		service = data;
	}

	@GetMapping(value = "/json", produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<Map<String, List<Data>>> json() {
		List<Data> rows = new ArrayList<>();
		rows.add(new Data(service.hello()));
		Map<String, List<Data>> results = new HashMap<>();
		results.put("results", rows);
		System.out.println("Returning: " + gson.toJson( results));
		return ResponseEntity.status(HttpStatus.OK).body(service.handleData(results));
	}

	public static class Data {

		private String text;

		public String getText() {
			return text;
		}

		public void setText(String data) {
			text = data;
		}

		public Data(String text) {
			this.text = text;
		}

		public Data() {
		}
	}
}
