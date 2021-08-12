package example.controller;
/**
 * Copyright 2021 Serguei Kouzmine
 */

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import example.component.SearchRequest;
import example.component.SearchResponseRow;
import example.component.TagKey;
import example.service.ExampleService;

// https://grafana.com/grafana/plugins/grafana-simple-json-datasource/
@RestController
@RequestMapping("/")
public class TagController {

	// @Autowired
	private ExampleService service;

	// for mocking
	public TagController(ExampleService data) {
		service = data;
	}

	// NOTE: do not have any use of payload, but still ask for a valid JSON
	@RequestMapping(method = RequestMethod.POST, value = "tag-keys", produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<List<TagKey>> postTagKeysRequest(
			@RequestBody Object data) {
		List<TagKey> result = new ArrayList<>();
		TagKey row = new TagKey();
		row.setText("text data");
		row.setType("value data");
		result.add(row);
		final HttpHeaders headers = Utils.addResponseHeaders();
		return ResponseEntity.status(HttpStatus.OK).headers(headers)
				.contentType(MediaType.APPLICATION_JSON).body(result);
	}

	// just for sake of variation accept JSON any map
	@RequestMapping(method = RequestMethod.POST, value = "tag-values", produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<List<Map<String, String>>> postTagValuesRequest(
			@RequestBody Map<String, String> request) {
		List<Map<String, String>> result = new ArrayList<>();
		Map<String, String> row = new HashMap<>();
		row.put("text", "one");
		result.add(row);
		row = new HashMap<>();
		row.put("text", "two");
		// NOTE: clead() will lead to repetition - there will be no "two" in the
		// response
		result.add(row);
		row.clear();
		row.put("text", "three");
		result.add(row);
		final HttpHeaders headers = Utils.addResponseHeaders();
		return ResponseEntity.status(HttpStatus.OK).headers(headers)
				.contentType(MediaType.APPLICATION_JSON).body(result);
	}
}
