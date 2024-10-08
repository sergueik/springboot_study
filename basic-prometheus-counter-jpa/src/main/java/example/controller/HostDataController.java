package example.controller;

/**
 * Copyright 2022 Serguei Kouzmine
 */

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;

import org.springframework.web.bind.annotation.RestController;

import example.service.HostData;

@RestController
@RequestMapping("/")
public class HostDataController {

	private static final Logger logger = LogManager
			.getLogger(HostDataController.class);

	@Value("#{'${example.booleans}'.split(',')}")
	private boolean[] booleansFlagArray;

	@Value("#{${example.metricExtractors}}")
	private Map<String, String> metricExtractors;

	@Value("#{'${example.metricNames}'.split(',')}")
	private String[] metricNames;

	// NOTE: no need to split: parses fine without it
	// @Value("#{'${example.booleans}'.split(',')}")
	@Value("#{'${example.booleans}'}")
	private List<Boolean> booleansFlagsList;

	private static final boolean debug = true;

	@ResponseBody
	@GetMapping(value = "hostdata/{hostname}", produces = MediaType.TEXT_PLAIN_VALUE)
	public ResponseEntity<Map<String, Object>> hostdata(
			@PathVariable String hostname) {
		if (debug)
			logger.info("process hostdata for " + hostname);
		HostData hostData = new HostData(hostname);
		hostData.setMetrics(Arrays.asList(metricNames));
		hostData.setMetricExtractors(metricExtractors);
		hostData.readData();
		if (debug) {
			logger.info("booleansFlagArray: {}", booleansFlagArray);
			// booleansFlagArray: [true, true, true, false, true]
			logger.info("booleansFlagsList: {}", booleansFlagsList);
			// booleansFlagsList: [true, true, true, false, true]
		}
		Map<String, String> data = hostData.getData();
		Map<String, Object> result = new HashMap<>();
		result.put("hostname", hostname);
		result.put("data", data);
		// NOTE: org.springframework.http.converter.HttpMessageNotWritableException:
		// No converter for [class java.util.HashMap]
		// with preset Content-Type 'null'
		return ResponseEntity.ok().contentType(MediaType.APPLICATION_JSON)
				.body(result);
	}
}
