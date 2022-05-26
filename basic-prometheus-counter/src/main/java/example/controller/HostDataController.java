package example.controller;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;

/**
 * Copyright 2022 Serguei Kouzmine
 */

import org.springframework.web.bind.annotation.RestController;

import example.service.HostData;

@RestController
@RequestMapping("/")
public class HostDataController {

	private static final Logger logger = LogManager
			.getLogger(HostDataController.class);

	@Value("#{${example.metricExtractors}}")
	private Map<String, String> metricExtractors;

	@Value("#{'${example.metricNames}'.split(',')}")
	private String[] metricNames;

	private static final boolean debug = false;

	@Autowired
	private HostData hostData;

	@ResponseBody
	@GetMapping(value = "hostdata/{hostname}", produces = MediaType.TEXT_PLAIN_VALUE)
	public ResponseEntity<Map<String, Object>> hostdata(
			@PathVariable String hostname) {
		logger.info("process hostdata for " + hostname);
		hostData.setHostname(hostname);
		Map<String, String> metricTaker = new HashMap<>(); // currently unused

		metricTaker.put("load_average",
				"\\s*(?:\\S+)\\s\\s*(?:\\S+)\\s\\s*(?:\\S+)\\s\\s*(?:\\S+)\\s\\s*(\\S+)\\s*");

		hostData.setMetrics(Arrays.asList(metricNames));
		hostData.setMetricExtractors(metricExtractors);
		hostData.readData();

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
