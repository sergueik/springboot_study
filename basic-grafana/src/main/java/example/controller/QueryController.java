package example.controller;
/**
 * Copyright 2021 Serguei Kouzmine
 */

import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONException;
import org.json.JSONObject;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import example.component.QueryRequest;
import example.component.QueryTimeserieResponse;
import example.component.SearchRequest;
import example.service.ExampleService;

// https://grafana.com/grafana/plugins/grafana-simple-json-datasource/
@RestController
@RequestMapping("/")
public class QueryController {

	private final static Long high = 42L;
	private final static Long low = 26L;

	// @Autowired
	private ExampleService service;

	// for mocking
	public QueryController(ExampleService data) {
		service = data;
	}

	// array response
	@RequestMapping(method = RequestMethod.POST, value = "query", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
	public List<QueryTimeserieResponse> postQueryRequest(
			@RequestBody QueryRequest request) {
		logger.info("processing POST /query");
		List<QueryTimeserieResponse> result = new ArrayList<>();
		QueryTimeserieResponse row = new QueryTimeserieResponse();
		List<List<Double>> datapoints = new ArrayList<>();
		List<Double> datapoint = Arrays.asList((double) 622.0,
				(double) 1450754160000.0);
		String target = request.getTargets().get(0).getTarget();
		datapoints.add(datapoint);
		result.add(row);
		row.setTarget(target);
		row.setDatapoints(datapoints);
		return result;
	}

	/*
	 java.lang.IllegalStateException: Ambiguous mapping. Cannot map 'example.controller.QueryController@6db66836' method 
	public java.util.List<example.component.QueryTimeserieResponse> example.controller.QueryController.postSearchRequest(example.component.QueryRequest)
	to {[/query],methods=[POST],consumes=[application/json],produces=[application/json]}: There is already 'example.controller.QueryController@6db66836' bean method
	public org.springframework.http.ResponseEntity<java.util.List<java.util.Map<java.lang.String, java.lang.Object>>> example.controller.QueryController.Query(java.util.Map<java.lang.String, java.lang.Object>) throws org.json.JSONException mapped.	
	 */
	private static final Logger logger = LogManager
			.getLogger(QueryController.class);

	// not strongly typed legacy code.
	// NOTE: Need to map to different route
	@SuppressWarnings("unchecked")
	@RequestMapping(value = "query_legacy", method = RequestMethod.POST, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
	@ResponseBody
	public ResponseEntity<List<Map<String, Object>>> Query(
			@RequestBody Map<String, Object> params) throws JSONException {

		logger
				.info("processing POST /query\n" + new JSONObject(params).toString(4));
		List<Map<String, String>> targets = (List<Map<String, String>>) params
				.get("targets");

		Map<String, String> range = (Map<String, String>) params.get("range");
		String from = range.get("from");
		String to = range.get("to");
		Long fromEpochMillisec = 1000 * Instant.parse(from).getEpochSecond();
		Long toEpochMillisecond = 1000 * Instant.parse(to).getEpochSecond();
		List<Map<String, Object>> result = new ArrayList<>();
		for (Map<String, String> targetMap : targets) {
			String target = (String) targetMap.get("target");
			Map<String, Object> resultRow = new HashMap<>();
			List<Long> datapoint1 = new ArrayList<Long>();
			datapoint1.add(high);
			datapoint1.add(fromEpochMillisec);
			List<Long> datapoint2 = new ArrayList<Long>();
			datapoint2.add(low);
			datapoint2.add(toEpochMillisecond);

			List<List<Long>> datapoints = new ArrayList<>();
			datapoints.add(datapoint1);
			datapoints.add(datapoint2);
			resultRow.put("datapoints", datapoints);

			resultRow.put("target", target);
			switch (target) {
			case "data series":
				result.add(resultRow);
				break;
			default:
			}
		}
		final HttpHeaders headers = new HttpHeaders();
		Collections.sort(result, (o1, o2) -> {
			String name1 = String.valueOf(o1.get("target").toString());
			String name2 = String.valueOf(o2.get("target").toString());
			return name1.compareTo(name2);
		});

		return ResponseEntity.status(HttpStatus.OK).headers(headers)
				.contentType(MediaType.TEXT_PLAIN).body(result);

	}

}
