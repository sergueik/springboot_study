package example.controller;

/**
 * Copyright 2021 Serguei Kouzmine
 */

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
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
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import example.model.DataRow;
import example.service.ExampleService;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonSerializationContext;
import com.google.gson.JsonSerializer;

@RestController
@RequestMapping("/")
public class ExampleRestController {

	private static Gson gson = new Gson();
	@Autowired
	private ExampleService service;

	public ExampleRestController(ExampleService data) {
		service = data;
	}

	@SuppressWarnings("serial")
	@GetMapping(value = "select", produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<List<String>> select() {
		List<String> results = new ArrayList<>();
		// https://www.baeldung.com/java-initialize-hashset
		results.addAll(new HashSet<String>() {
			{
				add("alice");
				add("bob");
				add("carl");
			}
		});
		System.out.println("Returning: " + gson.toJson(results));
		return ResponseEntity.status(HttpStatus.OK).body(results);
	}

	@GetMapping(value = "list", produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<Map<String, List<Data>>> list(
			@RequestParam Integer total) {
		List<Data> rows = new ArrayList<>();
		for (int cnt = 0; cnt != total; cnt++) {
			rows.add(new Data(String.format("%s %d", service.hello(), cnt + 1)));
		}
		Map<String, List<Data>> results = new HashMap<>();
		results.put("results", rows);
		System.out.println("Returning: " + gson.toJson(results));
		return ResponseEntity.status(HttpStatus.OK)
				.body(service.handleData(results));
	}

	@GetMapping(value = "table", produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<Map<String, List<DataRow>>> table() {
		List<DataRow> rows = new ArrayList<>();
		DataRow.DataRowBuilder rowBuilder = new DataRow.DataRowBuilder();
		for (int cnt = 1; cnt != 5; cnt++) {
			rowBuilder.withColumn1(String.format("row %d column 1", cnt));
			rowBuilder.withColumn2(String.format("row %d column 2", cnt));
			rowBuilder.withColumn3(String.format("row %d column 3", cnt));
			rows.add(rowBuilder.build());
		}
		Map<String, List<DataRow>> results = new HashMap<>();
		results.put("results", rows);
		System.out.println("Returning: " + gson.toJson(results));
		return ResponseEntity.status(HttpStatus.OK).body(results);
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
	// NOTE: are nested static classes supported?
}
