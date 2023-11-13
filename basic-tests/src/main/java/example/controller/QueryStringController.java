package example.controller;
/**
 * Copyright 2023 Serguei Kouzmine
 */

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
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

	// origin: https://github.com/skkovalenko/ToDoList
	@PutMapping(value = "data/{id}", consumes = MediaType.APPLICATION_FORM_URLENCODED_VALUE)
	public ResponseEntity<String> data(@RequestParam Map<String, String> mapParam,
			@PathVariable int id) {
		// mock retrieving a instance of typed "TODO-LIST" element from
		// repository elementRepository by id
		Optional<Map<String, String>> element = Optional
				.of(new HashMap<String, String>());
		if (!element.isPresent()) {
			return ResponseEntity.status(HttpStatus.NOT_FOUND).body(null);
		}
		data = element.get();
		data.put("name", mapParam.get("name"));
		data.put("description", mapParam.get("description"));
		data.put("date", mapParam.get("date"));
		data.put("id", String.format("%d", id));
		element = Optional.of(data);
		// elementRepository.save(element.get());
		payload = gson.toJson(data);
		return ResponseEntity.ok(payload);
	}

}
