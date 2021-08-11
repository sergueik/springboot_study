package example.controller;
/**
 * Copyright 2021 Serguei Kouzmine
 */

import java.util.ArrayList;
import java.util.List;

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
import example.service.ExampleService;

// https://grafana.com/grafana/plugins/grafana-simple-json-datasource/
@RestController
@RequestMapping("/")
public class SearchController {

	// @Autowired
	private ExampleService service;

	// for mocking
	public SearchController(ExampleService data) {
		service = data;
	}

	private static final Logger logger = LogManager
			.getLogger(SearchController.class);

	// array response
	@RequestMapping(method = RequestMethod.POST, value = "/search", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
	public List<String> postSearchRequest(@RequestBody SearchRequest data) {
		List<String> result = new ArrayList<>();
		result.add("test");
		return result;
	}

	// map response
	// NOTE: currently cannot distinguish
	@RequestMapping(method = RequestMethod.POST, value = "/search2", produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<List<SearchResponseRow>> postSearch2Request(
			@RequestBody SearchRequest data) {
		List<SearchResponseRow> result = new ArrayList<>();
		SearchResponseRow row = new SearchResponseRow();
		row.setText("text data");
		row.setValue("value data");
		result.add(row);
		final HttpHeaders headers = Utils.addResponseHeaders();
		return ResponseEntity.status(HttpStatus.OK).headers(headers)
				.contentType(MediaType.APPLICATION_JSON).body(result);

	}

	@RequestMapping(value = "/search", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
	@ResponseBody
	public List<String> Search(HttpServletResponse response) {
		logger.info("processing POST /search");
		Utils.addResponseHeaders(response);
		/*
		response.setHeader("Access-Control-Allow-Headers", "accept, content-type");
		response.setHeader("Access-Control-Allow-Methods", "POST");
		response.setHeader("Access-Control-Allow-Origin", "*");
		*/
		List<String> result = new ArrayList<String>();
		result.add("data series");
		return result;
	}
}
