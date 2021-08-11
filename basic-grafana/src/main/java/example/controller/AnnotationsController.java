package example.controller;
/**
 * Copyright 2021 Serguei Kouzmine
 */

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import example.component.AnnotationRequest;
import example.component.AnnotationResponseRow;
import example.service.ExampleService;

// https://grafana.com/grafana/plugins/grafana-simple-json-datasource/
@RestController
@RequestMapping("/")
public class AnnotationsController {

	// @Autowired
	private ExampleService service;

	// for mocking
	public AnnotationsController(ExampleService data) {
		service = data;
	}

	private static final Logger logger = LogManager
			.getLogger(AnnotationsController.class);

	@RequestMapping(method = RequestMethod.POST, value = "/annotations", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
	public List<AnnotationResponseRow> postAnnotationRequest(
			@RequestBody AnnotationRequest data) {
		List<AnnotationResponseRow> response = new ArrayList<>();
		AnnotationResponseRow row = new AnnotationResponseRow();
		row.setAnnotation(data.getAnnotation());
		response.add(row);
		// adding an empty row
		response.add(new AnnotationResponseRow());
		return response;

	}

	// not strongly typed, legacy
	@RequestMapping(value = "/annotations", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
	@ResponseBody
	public ResponseEntity<Map<String, String>> annotations() {
		logger.info("processing POST /annotations");
		Map<String, String> data = new HashMap<>();
		data.put("result", "OK");

		return ResponseEntity.status(HttpStatus.OK)
				.header("Access-Control-Allow-Headers", "accept, content-type")
				.header("Access-Control-Allow-Methods", "POST")
				.header("Access-Control-Allow-Origin", "*")
				.contentType(MediaType.APPLICATION_JSON_UTF8).body(data);
	}
}
