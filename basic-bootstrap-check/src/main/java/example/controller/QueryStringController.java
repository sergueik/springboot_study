package example.controller;
/**
 * Copyright 2023 Serguei Kouzmine
 */

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.google.gson.Gson;

@RestController
@RequestMapping("/basic")
public class QueryStringController {

	private Map<String, String> data = new HashMap<>();
	private final Gson gson = new Gson();
	private String payload;

	// based on: https://github.com/skkovalenko/ToDoList
	@PutMapping(value = "data/{id}")
	public ResponseEntity<String> put(@RequestParam Map<String, String> mapParam,
			@PathVariable int id) {
		// mock retrieving a instance of typed "TODO-LIST" element from
		// repository elementRepository by id
		Optional<Map<String, String>> element = Optional
				.of(new HashMap<String, String>());
		if (!element.isPresent()) {
			return ResponseEntity.status(HttpStatus.NOT_FOUND).body(null);
		}
		System.err.println("put Param: " + Arrays.asList(mapParam.keySet()));
		data = element.get();
		// clone the request param
		data = element.get();
		for (String key : mapParam.keySet()) {
			data.put(key, mapParam.get(key));
		}
		data.put("id", String.format("%d", id));
		element = Optional.of(data);
		// elementRepository.save(element.get());
		payload = gson.toJson(data);
		System.err.println("put return: " + payload);
		return ResponseEntity.ok(payload);
	}

	// based on: https://github.com/skkovalenko/ToDoList
	@GetMapping(value = "data/{id}")
	public ResponseEntity<String> get(@RequestParam Map<String, String> mapParam,
			@PathVariable int id) {
		// mock retrieving a instance of typed "TODO-LIST" element from
		// repository elementRepository by id
		System.err.println("get Param: " + Arrays.asList(mapParam.keySet()));
		Optional<Map<String, String>> element = Optional
				.of(new HashMap<String, String>());
		if (!element.isPresent()) {
			return ResponseEntity.status(HttpStatus.NOT_FOUND).body(null);
		}
		// clone the request param
		data = element.get();
		for (String key : mapParam.keySet()) {
			data.put(key, mapParam.get(key));
		}
		data.put("id", String.format("%d", id));
		element = Optional.of(data);
		// elementRepository.save(element.get());
		payload = gson.toJson(data);
		System.err.println("get return: " + payload);
		return ResponseEntity.ok(payload);
	}

}
